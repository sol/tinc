
module Graph (topolocicalOrder, fromDot) where

import           Control.Monad
import           Data.Graph.Wrapper as G
import           Data.Map as Map (empty, Map, toList, alter)
import           Data.Maybe
import           Language.Dot.Parser as Dot
import           Language.Dot.Syntax as Dot
import           Text.Parsec.Error

topolocicalOrder :: G.Graph String String -> [String]
topolocicalOrder g = map (vertex g) (topologicalSort g)

fromDot :: String -> Either String (G.Graph String String)
fromDot dot = case parseDot "<input>" dot of
  Right (Dot.Graph _ _ _ statements) ->
    fmap fromMap $
    foldM collectStatements Map.empty statements
  Left parseError -> Left $ unlines $ map messageString $ errorMessages parseError

collectStatements :: Map String [String] -> Statement
  -> Either String (Map String [String])
collectStatements acc s = case s of
  NodeStatement a _ ->
    return $ alter (Just . fromMaybe []) (toString a) acc
  EdgeStatement [ENodeId _ a, ENodeId _ b] _ ->
    return $
    alter (Just . fromMaybe []) (toString b) $
    alter (fmap (toString b :) . Just . fromMaybe []) (toString a) acc
  x -> Left ("unsupported dot statements: " ++ show x)

fromMap :: Ord a => Map a [a] -> G.Graph a a
fromMap m = G.fromListSimple $ Map.toList m

toString :: NodeId -> String
toString (NodeId i _) = case i of
  NameId s -> s
  StringId s -> s
  IntegerId i -> show i
  FloatId f -> show f
  x@XmlId{} -> show x
