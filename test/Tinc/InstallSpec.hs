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

    describe "findReusablePackages" $ do
    {-
      it "" $ do
        pending
        findReusablePackages cache [genericsSop] `shouldReturn`
          ([], ["/tmp/tinc-test-cache/tinc-getopt-generics/.cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d/generics-sop-0.1.1.2-ea79b4f97618c1d053ba280adf4ba238.conf"])
          -}

      it "" $ do
        findReusablePackages cache [hspecDiscover] `shouldReturn` ([], ["hspec-discover.conf"])

{-
      it "" $ do
        pending
        findReusablePackages cache [genericsSop, setenv] `shouldReturn` ([],[
            "/tmp/tinc-test-cache/tinc-getopt-generics/.cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d/generics-sop-0.1.1.2-ea79b4f97618c1d053ba280adf4ba238.conf"
          , "/tmp/tinc-test-cache/tinc-setenv/.cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d/setenv-0.1.1.3-c06acd2a7950de6c8bef6488f34c8beb.conf"
          ])

      it "" $ do
        pending
        findReusablePackages cache [getoptGenerics] `shouldReturn`
          ([getoptGenerics], [])
          -}

    describe "installDependencies" $ do
      let listPackages = readProcess "cabal" (words "exec ghc-pkg list") ""
          packageImportDirs package = readProcess "cabal" ["exec", "ghc-pkg", "field", package, "import-dirs"] ""

      it "populates cache" $ do
        inTempDirectoryNamed "foo" $ do
          writeFile "foo.cabal" . unlines $ cabalFile ++ ["    , setenv == 0.1.1.3"]
          removeDirectory setenvSandbox
          silence $ installDependencies False cache
          packageImportDirs "setenv" >>= (`shouldContain` path cache)

      it "reuses packages" $ do
        inTempDirectoryNamed "foo" $ do
          writeFile "foo.cabal" . unlines $ cabalFile ++ ["    , setenv == 0.1.1.3"]
          silence $ installDependencies False cache
          ghcPkgCheck
          doesDirectoryExist cabalSandboxDirectory `shouldReturn` True
          packageImportDirs "generics-sop" >>= (`shouldContain` path getoptGenericsSandbox)
          packageImportDirs "setenv" >>= (`shouldContain` path setenvSandbox)

      it "skips redundant packages" $ do
        inTempDirectoryNamed "foo" $ do
          writeFile "foo.cabal" $ unlines cabalFile
          silence $ installDependencies False cache
          listPackages >>= (`shouldNotContain` showPackage getoptGenerics)

      it "is idempotent" $ do
        inTempDirectoryNamed "foo" $ do
          writeFile "foo.cabal" $ unlines cabalFile
          silence $ installDependencies False cache
          xs <- getDirectoryContents (path cache)
          silence $ installDependencies False cache
          ys <- getDirectoryContents (path cache)
          ys `shouldMatchList` xs

    describe "realizeInstallPlan" $ do
      context "with --dry-run" $ do
        it "does not create a sandbox" $ do
          inTempDirectoryNamed "foo" $ do
            writeFile "foo.cabal" (unlines cabalFile)
            silence $ realizeInstallPlan True cache []
            doesDirectoryExist cabalSandboxDirectory `shouldReturn` False

        it "does not delete an existing sandbox" $ do
          inTempDirectory $ do
            writeFile "foo.cabal" (unlines cabalFile)
            touch "cabal.sandbox.config"
            touch ".cabal-sandbox/foo"
            silence $ realizeInstallPlan True cache []
            doesFileExist ".cabal-sandbox/foo" `shouldReturn` True

ghcPkgCheck :: IO ()
ghcPkgCheck = hSilence [stderr] $ callCommand "cabal exec ghc-pkg check"
