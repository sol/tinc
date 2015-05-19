
module Graph (topolocicalOrder, fromDot) where

import           Data.Graph.Wrapper as G
import           Data.List
import           Data.Map as Map (empty, Map, toList, alter)
import           Data.Maybe
import           Language.Dot.Parser as Dot
import           Language.Dot.Syntax as Dot

topolocicalOrder :: G.Graph String String -> [String]
topolocicalOrder g = map (vertex g) (topologicalSort g)

fromDot :: String -> G.Graph String String
fromDot dot = case parseDot "<input>" dot of
  Right (Dot.Graph _ _ _ statements) ->
    fromMap $
    foldl' collectStatements Map.empty statements

collectStatements :: Map String [String] -> Statement -> Map String [String]
collectStatements acc s = case s of
  NodeStatement a _ ->
    alter (Just . fromMaybe []) (toString a) acc
  EdgeStatement [ENodeId _ a, ENodeId _ b] _ ->
    alter (Just . fromMaybe []) (toString b) $
    alter (fmap (toString b :) . Just . fromMaybe []) (toString a) acc
  x -> error (show x)

fromMap :: Ord a => Map a [a] -> G.Graph a a
fromMap m = G.fromListSimple $ Map.toList m

toString :: NodeId -> String
toString (NodeId i _) = case i of
  NameId s -> s
  StringId s -> s
  x -> error ("Graph.toString: " ++ show x)
