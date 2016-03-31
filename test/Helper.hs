{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Helper (
  module Test.Hspec
, module Test.Mockery.Directory
, facts
, skipForGhc78
) where

import           Test.Hspec
import           Test.Mockery.Directory

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

skipForGhc78 :: Expectation -> Expectation
#if __GLASGOW_HASKELL__ < 710
skipForGhc78 _ = pending
#else
skipForGhc78 = id
#endif
