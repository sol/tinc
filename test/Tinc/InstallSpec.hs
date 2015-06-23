{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tinc.InstallSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Helper

import           System.Directory hiding (removeDirectory)
import           System.IO
import           System.IO.Silently
import           System.Process
import           Test.Hspec.Expectations.Contrib
import           Test.Mockery.Directory

import           Package
import           Tinc.Types
import           Tinc.Install
import           Tinc.GhcInfo
import           Tinc.Cache

cabalFile :: [String]
cabalFile =
  [ "name:           foo"
  , "version:        0.0.0"
  , "cabal-version:  >= 1.8"
  , "library"
  , "  build-depends:"
  , "      generics-sop"
  ]

spec :: Spec
spec = before_ ensureCache $ do
  beforeAll getGhcInfo $ do
    describe "realizeInstallPlan" $ do
      let listPackages = readProcess "cabal" (words "exec ghc-pkg list") ""
          packageImportDirs package = readProcess "cabal" ["exec", "ghc-pkg", "field", package, "import-dirs"] ""

      it "populates cache" $ \ ghcInfo -> do
        inTempDirectoryNamed "foo" $ do
          writeFile "foo.cabal" . unlines $ cabalFile ++ ["    , setenv == 0.1.1.3"]
          removeDirectory setenvSandbox
          silence $ realizeInstallPlan ghcInfo False cacheDir [genericsSop, setenv]
          packageImportDirs "setenv" >>= (`shouldContain` path cacheDir)

      it "reuses packages" $ \ ghcInfo -> do
        inTempDirectoryNamed "foo" $ do
          writeFile "foo.cabal" . unlines $ cabalFile ++ ["    , setenv == 0.1.1.3"]
          silence $ realizeInstallPlan ghcInfo False cacheDir [genericsSop, setenv]
          ghcPkgCheck
          doesDirectoryExist cabalSandboxDirectory `shouldReturn` True
          packageImportDirs "generics-sop" >>= (`shouldContain` path getoptGenericsSandbox)
          packageImportDirs "setenv" >>= (`shouldContain` path setenvSandbox)

      it "skips redundant packages" $ \ ghcInfo -> do
        inTempDirectoryNamed "foo" $ do
          writeFile "foo.cabal" $ unlines cabalFile
          silence $ realizeInstallPlan ghcInfo False cacheDir [genericsSop]
          listPackages >>= (`shouldNotContain` showPackage getoptGenerics)

      it "is idempotent" $ \ ghcInfo -> do
        inTempDirectoryNamed "foo" $ do
          writeFile "foo.cabal" $ unlines cabalFile
          silence $ realizeInstallPlan ghcInfo False cacheDir [genericsSop]
          xs <- getDirectoryContents (path cacheDir)
          silence $ realizeInstallPlan ghcInfo False cacheDir [genericsSop]
          ys <- getDirectoryContents (path cacheDir)
          ys `shouldMatchList` xs

      context "with --dry-run" $ do
        it "does not create a sandbox" $ \ ghcInfo -> do
          inTempDirectoryNamed "foo" $ do
            writeFile "foo.cabal" (unlines cabalFile)
            silence $ realizeInstallPlan ghcInfo True cacheDir []
            doesDirectoryExist cabalSandboxDirectory `shouldReturn` False

        it "does not delete an existing sandbox" $ \ ghcInfo -> do
          inTempDirectory $ do
            writeFile "foo.cabal" (unlines cabalFile)
            touch "cabal.sandbox.config"
            touch ".cabal-sandbox/foo"
            silence $ realizeInstallPlan ghcInfo True cacheDir []
            doesFileExist ".cabal-sandbox/foo" `shouldReturn` True

ghcPkgCheck :: IO ()
ghcPkgCheck = hSilence [stderr] $ callCommand "cabal exec ghc-pkg check"
