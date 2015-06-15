{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tinc.InstallSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Helper

import           Data.List.Compat
import           System.Directory hiding (removeDirectory)
import           System.FilePath
import           System.IO
import           System.IO.Silently
import           System.Process
import           Test.Hspec.Expectations.Contrib
import           Test.Mockery.Directory

import           Package
import           Tinc.Types
import           Tinc.Install
import           Tinc.GhcInfo

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
    describe "findPackageDB" $ do
      it "finds the sandbox package db" $ do
        r <- findPackageDB getoptGenericsSandbox
        path r `shouldSatisfy` (\ p -> (path getoptGenericsSandbox </> cabalSandboxDirectory) `isPrefixOf` p && isPackageDB p)

      it "returns an absolute path" $ do
        r <- findPackageDB getoptGenericsSandbox
        path r `shouldSatisfy` ("/" `isPrefixOf`)

    describe "extractPackages" $ do
      it "extracts the packages" $ do
        packageDB <- findPackageDB getoptGenericsSandbox
        packages <- extractPackages packageDB
        packages `shouldSatisfy` any (("tagged" `isInfixOf`) . path)
        packages `shouldSatisfy` all (("/" `isPrefixOf`) . path)

    beforeAll getGhcInfo $ do
      describe "realizeInstallPlan" $ do
        let listPackages = readProcess "cabal" (words "exec ghc-pkg list") ""
            packageImportDirs package = readProcess "cabal" ["exec", "ghc-pkg", "field", package, "import-dirs"] ""

        it "populates cache" $ \ ghcInfo -> do
          inTempDirectoryNamed "foo" $ do
            writeFile "foo.cabal" . unlines $ cabalFile ++ ["    , setenv == 0.1.1.3"]
            removeDirectory setenvSandbox
            silence $ realizeInstallPlan ghcInfo False cache [genericsSop, setenv]
            packageImportDirs "setenv" >>= (`shouldContain` path cache)

        it "reuses packages" $ \ ghcInfo -> do
          inTempDirectoryNamed "foo" $ do
            writeFile "foo.cabal" . unlines $ cabalFile ++ ["    , setenv == 0.1.1.3"]
            silence $ realizeInstallPlan ghcInfo False cache [genericsSop, setenv]
            ghcPkgCheck
            doesDirectoryExist cabalSandboxDirectory `shouldReturn` True
            packageImportDirs "generics-sop" >>= (`shouldContain` path getoptGenericsSandbox)
            packageImportDirs "setenv" >>= (`shouldContain` path setenvSandbox)

        it "skips redundant packages" $ \ ghcInfo -> do
          inTempDirectoryNamed "foo" $ do
            writeFile "foo.cabal" $ unlines cabalFile
            silence $ realizeInstallPlan ghcInfo False cache [genericsSop]
            listPackages >>= (`shouldNotContain` showPackage getoptGenerics)

        it "is idempotent" $ \ ghcInfo -> do
          inTempDirectoryNamed "foo" $ do
            writeFile "foo.cabal" $ unlines cabalFile
            silence $ realizeInstallPlan ghcInfo False cache [genericsSop]
            xs <- getDirectoryContents (path cache)
            silence $ realizeInstallPlan ghcInfo False cache [genericsSop]
            ys <- getDirectoryContents (path cache)
            ys `shouldMatchList` xs

        context "with --dry-run" $ do
          it "does not create a sandbox" $ \ ghcInfo -> do
            inTempDirectoryNamed "foo" $ do
              writeFile "foo.cabal" (unlines cabalFile)
              silence $ realizeInstallPlan ghcInfo True cache []
              doesDirectoryExist cabalSandboxDirectory `shouldReturn` False

          it "does not delete an existing sandbox" $ \ ghcInfo -> do
            inTempDirectory $ do
              writeFile "foo.cabal" (unlines cabalFile)
              touch "cabal.sandbox.config"
              touch ".cabal-sandbox/foo"
              silence $ realizeInstallPlan ghcInfo True cache []
              doesFileExist ".cabal-sandbox/foo" `shouldReturn` True

ghcPkgCheck :: IO ()
ghcPkgCheck = hSilence [stderr] $ callCommand "cabal exec ghc-pkg check"
