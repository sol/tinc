{-# LANGUAGE OverloadedStrings #-}
module Tinc.CacheSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Helper

import           System.IO.Temp
import           System.Directory
import           System.FilePath

import           Tinc.Cache
import           Tinc.Types
import           Tinc.PackageGraph

spec :: Spec
spec = do
  describe "findPackageDb" $ do
    it "finds the sandbox package db" $ do
      withSystemTempDirectory "tinc" $ \ sandbox -> do
        let packageDb = sandbox </> ".cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d"
        createDirectoryIfMissing True packageDb
        findPackageDb (Path sandbox) `shouldReturn` (Path packageDb)

    context "when sandbox does not contain a package db" $ do
      it "throws an exception" $ do
        withSystemTempDirectory "tinc" $ \ sandbox -> do
          let p = sandbox </> ".cabal-sandbox"
          createDirectory p
          findPackageDb (Path sandbox) `shouldThrow` errorCall ("src/Tinc/Cache.hs: No package database found in " ++ show p)

  describe "readPackageGraph" $ beforeAll_ ensureCache $ do
    context "when a package has no dependencies and no other packages depend on it" $ do
      it "includes package" $ do
        -- NOTE: `ghc-pkg dot` omits packages from the graph that both:
        --
        -- 1. have no dependencies
        -- 2. no other packages depend on
        --
        -- This test case makes sure that we properly handle this.
        packageDb <- findPackageDb hspecDiscoverSandbox
        readPackageGraph [packageDb] `shouldReturn` toGraph [(hspecDiscover, [])]
