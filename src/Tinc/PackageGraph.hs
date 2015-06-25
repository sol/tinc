{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Tinc.PackageGraph (
  PackageGraph
, fromDot
, calculateReusablePackages
) where

import           Control.Monad
import           Data.Graph.Wrapper as G
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (member)
import qualified Data.Set as Set
import           Language.Dot.Parser as Dot
import qualified Language.Dot.Syntax as Dot
import           Language.Dot.Syntax hiding (Graph)
import           Text.Parsec.Error

import           Tinc.Package
import           Tinc.Fail

type PackageGraph a = Graph Package a

calculateReusablePackages :: Ord i => [i] -> Graph i v -> [(i, v)]
calculateReusablePackages installPlan cache = filter p cachedPackages
  where
    installPlanSet = Set.fromList installPlan

    cachedPackages = map f (toList cache)
      where f (i, v, _) = (i, v)

    p (package, _) =
      package `member` installPlanSet &&
      all (`member` installPlanSet) dependencies
      where
        dependencies = reachableVertices cache package

-- * dot

fromDot :: Fail m => [(Package, v)] -> String -> m (PackageGraph v)
fromDot values dot = case parseDot "<input>" dot of
  Right (Dot.Graph _ _ _ statements) ->
    fmap fromMap $
    foldM collectStatements (Map.fromList $ map (fmap (,[])) values) statements
  Left parseError -> dieLoc __FILE__ $ unlines $ map messageString $ errorMessages parseError

type PackageMap v = Map Package (v, [Package])

collectStatements :: Fail m => PackageMap v -> Statement -> m (PackageMap v)
collectStatements packageMap s = case s of
  NodeStatement (toPackage -> a) _ -> addDependencies a [] packageMap
  EdgeStatement [ENodeId _ (toPackage -> a), ENodeId _ (toPackage -> b)] _ ->
    addDependencies b [] packageMap >>= addDependencies a [b]
  x -> dieLoc __FILE__ ("Unsupported dot statements: " ++ show x)

addDependencies :: Fail m => (Ord i, Show i) => i -> [dep] -> Map i (v, [dep]) -> m (Map i (v, [dep]))
addDependencies package dependencies graph = case Map.lookup package graph of
  Nothing -> dieLoc __FILE__ ("No value for package: " ++ show package)
  Just (v, xs) -> return (Map.insert package (v, dependencies ++ xs) graph)

fromMap :: Ord i => Map i (v, [i]) -> Graph i v
fromMap = fromList . map f . Map.toList
  where
    f (i, (v, xs)) = (i, v, xs)

toPackage :: NodeId -> Package
toPackage (NodeId i _) = parsePackage $ case i of
  NameId s -> s
  StringId s -> s
  IntegerId int -> show int
  FloatId f -> show f
  x@XmlId{} -> show x
