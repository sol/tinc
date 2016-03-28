{-# OPTIONS_GHC -fno-warn-orphans #-}
module Helper (
  module Test.Hspec
, facts
) where

import           Test.Hspec

import qualified Data.Graph.Wrapper as G
import           Data.List

import           Tinc.Facts

instance (Ord a, Ord v) => Eq (G.Graph a v) where
  a == b = sort (G.toList a) == sort (G.toList b)

facts :: Facts
facts = Facts {
  factsCache = error "factsCache"
, factsAddSourceCache = error "factsAddSoruceCache"
, factsNixCache = error "factsNixCache"
, factsUseNix = False
, factsPlugins = []
, factsGhcInfo = error "factsGhcInfo"
}
