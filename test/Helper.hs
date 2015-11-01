{-# OPTIONS_GHC -fno-warn-orphans #-}
module Helper (
  module Test.Hspec
) where

import           Test.Hspec

import qualified Data.Graph.Wrapper as G
import           Data.List

instance (Ord a, Ord v) => Eq (G.Graph a v) where
  a == b = sort (G.toList a) == sort (G.toList b)
