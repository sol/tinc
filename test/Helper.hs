{-# OPTIONS_GHC -fno-warn-orphans #-}
module Helper (
  module Test.Hspec
, unlessTravis
) where

import           Test.Hspec

import qualified Data.Graph.Wrapper as G
import           Data.List
import           System.Environment

instance (Ord a, Ord v) => Eq (G.Graph a v) where
  a == b = sort (G.toList a) == sort (G.toList b)

unlessTravis :: IO () -> IO ()
unlessTravis action = do
  env <- getEnvironment
  case lookup "TRAVIS" env of
    Just _ -> return ()
    Nothing -> action
