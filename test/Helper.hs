{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Helper (
  module Test.Hspec
, module Test.Mockery.Directory
, facts

, module Tinc.Process
, process
) where

import           Test.Hspec
import           Test.Mockery.Directory
import           Test.Mockery.Action

import           Tinc.Facts
import           Tinc.Process hiding (process)

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

process :: HasCallStack => Process IO
process = Process {
  readProcess = dummy "readProcess"
, callProcess = dummy "callProcess"
}
