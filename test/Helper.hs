{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Helper (
  module Test.Hspec
, module Test.Mockery.Directory
, facts

, module Tinc.Process
, process

, skipForGhc78
) where

import           Test.Hspec
import           Test.Mockery.Directory
import           Test.Mockery.Action
import           Data.WithLocation

import qualified Data.Graph.Wrapper as G
import           Data.List

import           Tinc.Facts
import           Tinc.Process hiding (process)

instance (Ord a, Ord v) => Eq (G.Graph a v) where
  a == b = sort (G.toList a) == sort (G.toList b)

facts :: Facts
facts = Facts {
  factsCache = error "factsCache"
, factsGitCache = error "factsGitCache"
, factsAddSourceCache = error "factsAddSoruceCache"
, factsNixCache = error "factsNixCache"
, factsUseNix = False
, factsNixResolver = Nothing
, factsPlugins = []
, factsGhcInfo = error "factsGhcInfo"
}

process :: WithLocation (Process IO)
process = Process {
  readProcess = dummy "readProcess"
, callProcess = dummy "callProcess"
}

skipForGhc78 :: Expectation -> Expectation
#if __GLASGOW_HASKELL__ < 710
skipForGhc78 _ = pending
#else
skipForGhc78 = id
#endif
