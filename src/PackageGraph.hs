{-# LANGUAGE TupleSections #-}
module PackageGraph (PackageGraph, fromDot, calculateReusablePackages, toGraph) where

import           Control.Monad
import           Data.Graph.Wrapper as G
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (member)
import qualified Data.Set as Set
import           Language.Dot.Parser as Dot
import           Language.Dot.Syntax as Dot
import           Text.Parsec.Error

import           Package

type PackageGraph = G.Graph Package ()

calculateReusablePackages :: Ord a => [a] -> G.Graph a () -> [a]
calculateReusablePackages installPlan cache =
  filter p installPlan
  where
    installPlanSet = Set.fromList installPlan
    cacheSet = Set.fromList (vertices cache)
    p package =
      package `member` cacheSet &&
      all (`member` installPlanSet) dependencies
      where
        dependencies = reachableVertices cache package

-- * dot

fromDot :: [Package] -> String -> Either String PackageGraph
fromDot packages dot = case parseDot "<input>" dot of
  Right (Dot.Graph _ _ _ statements) ->
    fmap fromMap $
    foldM collectStatements (Map.fromList $ map (,[]) packages) statements
  Left parseError -> Left $ unlines $ map messageString $ errorMessages parseError

collectStatements :: Map Package [Package] -> Statement -> Either String (Map Package [Package])
collectStatements acc s = case s of
  NodeStatement a _ ->
    return $ insert (toPackage a) [] acc
  EdgeStatement [ENodeId _ a, ENodeId _ b] _ ->
    return $
    insert (toPackage b) [] $
    insert (toPackage a) [toPackage b] acc
  x -> Left ("unsupported dot statements: " ++ show x)
  where
    insert package dependencies = Map.insertWith (++) package dependencies

fromMap :: Ord a => Map a [a] -> G.Graph a ()
fromMap = toGraph . Map.toList

toGraph :: Ord a => [(a, [a])] -> G.Graph a ()
toGraph = void . fromListSimple

toPackage :: NodeId -> Package
toPackage (NodeId i _) = parsePackage $ case i of
  NameId s -> s
  StringId s -> s
  IntegerId int -> show int
  FloatId f -> show f
  x@XmlId{} -> show x
