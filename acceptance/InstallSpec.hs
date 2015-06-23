{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module InstallSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Helper

import           System.Directory hiding (removeDirectory)
import           System.Process
import           Test.Hspec.Expectations.Contrib
import           Test.Mockery.Directory
import           Data.String.Builder

import           Run
import           Package
import           Tinc.Types
import           Tinc.Install
import           Tinc.GhcInfo
import           Tinc.Cache

spec :: Spec
spec = do
  describe "installDependencies" $ do
    it "populates cache" $ do
      unsetEnvVars
      ensureCache
      ghcInfo <- getGhcInfo
      inTempDirectoryNamed "foo" $ do
        writeFile "foo.cabal" . build $ do
          "name:           foo"
          "version:        0.0.0"
          "cabal-version:  >= 1.8"
          "library"
          "  build-depends:"
          "      generics-sop"
          "    , setenv == 0.1.1.3"

        putStrLn "X populates cache"
        removeDirectory setenvSandbox
        installDependencies ghcInfo False cacheDir
        ghcPkgCheck
        packageImportDirs "setenv" >>= (`shouldContain` path cacheDir)

        putStrLn "X reuses packages"
        doesDirectoryExist cabalSandboxDirectory `shouldReturn` True
        packageImportDirs "generics-sop" >>= (`shouldContain` path getoptGenericsSandbox)

        putStrLn "X skips redundant packages"
        listPackages >>= (`shouldNotContain` showPackage getoptGenerics)

        putStrLn "X is idempotent"
        xs <- getDirectoryContents (path cacheDir)
        installDependencies ghcInfo False cacheDir
        ys <- getDirectoryContents (path cacheDir)
        ys `shouldMatchList` xs
  where
    listPackages = readProcess "cabal" (words "exec ghc-pkg list") ""
    packageImportDirs package = readProcess "cabal" ["exec", "ghc-pkg", "field", package, "import-dirs"] ""

    ghcPkgCheck :: IO ()
    ghcPkgCheck = callCommand "cabal exec ghc-pkg check"
