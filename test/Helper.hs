{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Helper (
  module Test.Hspec
, module Test.Mockery.Action
, facts
, dummyEnv
) where

import           Test.Hspec
import           Test.Mockery.Action

import qualified Data.Graph.Wrapper as G
import           Data.List
import           Data.WithLocation
import qualified Control.Exception as Exception

import           Tinc.Facts
import           Tinc.Env

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

dummyEnv :: WithLocation (Env IO)
dummyEnv = Env {
  envDoesFileExist = dummy "envDoesFileExist"
, envCopyFile = dummy "envCopyFile"
, envWithSystemTempDirectory = dummy "envWithSystemTempDirectory"
, envWithCurrentDirectory = dummy "envWithCurrentDirectory"

, envReadCabal = dummy "envReadCabal"

, envInitSandbox = dummy "envInitSandbox"

, envTry = Exception.try
, envThrowM = Exception.throwIO
}
