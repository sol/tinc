{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module AcceptanceSpec (main) where

import           Prelude ()
import           Prelude.Compat

import           Test.Hspec

import           Data.String.Builder
import           System.Directory hiding (removeDirectory, getDirectoryContents)
import           System.Process
import           Test.Mockery.Directory

import           Run (unsetEnvVars)
import           Tinc.GhcInfo
import           Tinc.Install
import           Tinc.Facts
import           Tinc.Types
import           Util

cacheDir :: Path CacheDir
cacheDir = "/tmp/tinc-test-cache"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "installDependencies" $ do
    it "populates cache" $ do
      unsetEnvVars

      ghcInfo <- getGhcInfo
      let tinc = installDependencies False Facts { factsGhcInfo = ghcInfo, factsCache = cacheDir, factsAddSourceCache = undefined, factsPlugins = undefined }

      createDirectoryIfMissing True (path cacheDir)

      inTempDirectoryNamed "getopt-generics" $ do
        writeFile "getopt-generics-cache.cabal" . build $ do
          "name:           foo"
          "version:        0.0.0"
          "cabal-version:  >= 1.8"
          "library"
          "  build-depends:"
          "      generics-sop == 0.1.1.2"
          "    , base-compat == 0.8.2"
          "    , base-orphans == 0.3.2"
          "    , generics-sop == 0.1.1.2"
          "    , tagged == 0.8.0.1"
          "    , getopt-generics == 0.6.3"
        tinc

      inTempDirectoryNamed "foo" $ do
        writeFile "foo.cabal" . build $ do
          "name:           foo"
          "version:        0.0.0"
          "cabal-version:  >= 1.8"
          "library"
          "  build-depends:"
          "      generics-sop == 0.1.1.2"

        tinc
        ghcPkgCheck

        putStrLn "X reuses packages"
        doesDirectoryExist ".cabal-sandbox" `shouldReturn` True
        packageImportDirs "generics-sop" >>= (`shouldContain` "/tmp/tinc-test-cache/getopt-generics-")

        putStrLn "X skips redundant packages"
        packages <- listPackages
        packages `shouldContain` "generics-sop"
        packages `shouldNotContain` "getopt-generics"

        putStrLn "X is idempotent"
        xs <- getDirectoryContents (path cacheDir)
        tinc
        ys <- getDirectoryContents (path cacheDir)
        ys `shouldMatchList` xs
  where
    listPackages = readProcess "cabal" ["exec", "ghc-pkg", "list"] ""
    packageImportDirs package = readProcess "cabal" ["exec", "ghc-pkg", "field", package, "import-dirs"] ""

    ghcPkgCheck :: IO ()
    ghcPkgCheck = callCommand "cabal exec ghc-pkg check"
