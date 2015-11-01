{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module InstallSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Helper

import           Data.String.Builder
import           System.Directory hiding (removeDirectory)
import           System.Process
import           Test.Mockery.Directory

import           Run
import           Tinc.GhcInfo
import           Tinc.Install
import           Tinc.Package
import           Tinc.Types

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
          "      generics-sop == 0.1.1.2"
          "    , setenv == 0.1.1.3"

        putStrLn "X populates cache"
        removeDirectory setenvSandbox
        let action = installDependencies ghcInfo False cacheDir (error (__FILE__ ++ ": git cache"))
        action
        ghcPkgCheck
        packageImportDirs "setenv" >>= (`shouldContain` path cacheDir)

        putStrLn "X reuses packages"
        doesDirectoryExist ".cabal-sandbox" `shouldReturn` True
        packageImportDirs "generics-sop" >>= (`shouldContain` path getoptGenericsSandbox)

        putStrLn "X skips redundant packages"
        listPackages >>= (`shouldNotContain` showPackage getoptGenerics)

        putStrLn "X is idempotent"
        xs <- getDirectoryContents (path cacheDir)
        action
        ys <- getDirectoryContents (path cacheDir)
        ys `shouldMatchList` xs
  where
    listPackages = readProcess "cabal" (words "exec ghc-pkg list") ""
    packageImportDirs package = readProcess "cabal" ["exec", "ghc-pkg", "field", package, "import-dirs"] ""

    ghcPkgCheck :: IO ()
    ghcPkgCheck = callCommand "cabal exec ghc-pkg check"
