{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module StackSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad
import           Data.List.Compat
import           System.Directory hiding (removeDirectory)
import           System.Environment.Compat
import           System.FilePath
import           System.IO
import           System.IO.Silently
import           System.Process
import           Test.Hspec
import           Test.Hspec.Expectations.Contrib
import           Test.Mockery.Directory
import           Shelly (shelly, rm_rf, cp_r, touchfile)
import           Data.String

import           Util
import           Package
import           Stack

spec :: Spec
spec = beforeAll_ unsetEnvVars . beforeAll_ mkCache . before_ restoreCache $ do
    describe "findPackageDB" $ do
      it "finds the sandbox package db" $ do
        r <- findPackageDB getoptGenericsSandbox
        path r `shouldSatisfy` (\ p -> (path getoptGenericsSandbox </> ".cabal-sandbox") `isPrefixOf` p && isPackageDB p)

      it "returns an absolute path" $ do
        r <- findPackageDB getoptGenericsSandbox
        path r `shouldSatisfy` ("/" `isPrefixOf`)

    describe "extractPackages" $ do
      it "extracts the packages" $ do
        packageDB <- findPackageDB getoptGenericsSandbox
        packages <- extractPackages packageDB
        packages `shouldSatisfy` any (("tagged" `isInfixOf`) . path)
        packages `shouldSatisfy` all (("/" `isPrefixOf`) . path)

    describe "createStackedSandbox" $ do
      it "registers packages from one sandbox in another" $ do
        inTempDirectory $ do
          createStackedSandbox getoptGenericsSandbox
          output <- readProcess "cabal" (words "exec ghc-pkg list") ""
          output `shouldContain` showPackage getoptGenerics

      it "yields a working sandbox" $ do
        inTempDirectory $ do
          createStackedSandbox getoptGenericsSandbox
          ghcPkgCheck

    describe "installDependencies" $ do
      let cabalFile =
            [ "name:           foo"
            , "version:        0.0.0"
            , "cabal-version:  >= 1.8"
            , "library"
            , "  build-depends:"
            , "      generics-sop"
            ]
          listPackages = readProcess "cabal" (words "exec ghc-pkg list") ""
          packageImportDirs package = readProcess "cabal" ["exec", "ghc-pkg", "field", package, "import-dirs"] ""

      it "populates cache" $ do
        inTempDirectoryNamed "foo" $ do
          writeFile "foo.cabal" . unlines $ cabalFile ++ ["    , setenv == 0.1.1.3"]
          removeDirectory setenvSandbox
          installDependencies cache
          packageImportDirs "setenv" >>= (`shouldContain` path cache)

      it "reuses packages" $ do
        inTempDirectoryNamed "foo" $ do
          writeFile "foo.cabal" . unlines $ cabalFile ++ ["    , setenv == 0.1.1.3"]
          installDependencies cache
          packageImportDirs "generics-sop" >>= (`shouldContain` path getoptGenericsSandbox)
          packageImportDirs "setenv" >>= (`shouldContain` path setenvSandbox)

      it "skips redundant packages" $ do
        inTempDirectoryNamed "foo" $ do
          writeFile "foo.cabal" $ unlines cabalFile
          installDependencies cache
          listPackages >>= (`shouldNotContain` showPackage getoptGenerics)

unsetEnvVars :: IO ()
unsetEnvVars = do
  unsetEnv "CABAL_SANDBOX_CONFIG"
  unsetEnv "CABAL_SANDBOX_PACKAGE_PATH"
  unsetEnv "GHC_PACKAGE_PATH"

withDirectory :: FilePath -> IO a -> IO a
withDirectory dir action = do
  createDirectoryIfMissing True dir
  withCurrentDirectory dir action

ghcPkgCheck :: IO ()
ghcPkgCheck = hSilence [stderr] $ callCommand "cabal exec ghc-pkg check"

getoptGenerics :: Package
getoptGenerics = Package "getopt-generics" "0.6.3"

getoptGenericsPackages :: [Package]
getoptGenericsPackages = [
    Package "base-compat" "0.8.2"
  , Package "base-orphans" "0.3.2"
  , Package "generics-sop" "0.1.1.2"
  , Package "tagged" "0.8.0.1"
  , getoptGenerics
  ]

mkTestSandbox :: Path Cache -> String -> [Package] -> IO ()
mkTestSandbox cache pattern packages = do
  let sandbox = toSandbox pattern cache
  exists <- doesDirectoryExist $ path sandbox
  when (not exists) $ do
    createDirectoryIfMissing True $ path sandbox
    withDirectory (path sandbox) $ do
      callCommand "cabal sandbox init"
      callCommand ("cabal install --disable-library-profiling --disable-optimization --disable-documentation " ++
                   unwords (map showPackage packages))

mkCache :: IO ()
mkCache = do
  exists <- doesDirectoryExist (path cacheBackup)
  unless exists $ do
    removeDirectory cache
    createDirectory (path cache)
    mkTestSandbox cache "setenv" [Package "setenv" "0.1.1.3"]
    mkTestSandbox cache "getopt-generics" getoptGenericsPackages
    copyDirectory cache cacheBackup

restoreCache :: IO ()
restoreCache = do
  removeDirectory cache
  copyDirectory cacheBackup cache
  forM_ [getoptGenericsSandbox, setenvSandbox] $ \ sandbox -> do
    Path packageDB <- findPackageDB sandbox
    shelly $ touchfile $ fromString (packageDB </> "package.cache")

cache :: Path Cache
cache = "/tmp/tinc-test-cache"

cacheBackup :: Path Cache
cacheBackup = "cache-backup"

copyDirectory :: Path a -> Path a -> IO ()
copyDirectory src dst = shelly $ do
  cp_r (fromString $ path src) (fromString $ path dst)

removeDirectory :: Path a -> IO ()
removeDirectory dir = shelly $ do
  rm_rf (fromString $ path dir)

toSandbox :: String -> Path Cache -> Path Sandbox
toSandbox pattern (Path cache) = Path (cache </> "tinc-" ++ pattern)

setenvSandbox :: Path Sandbox
setenvSandbox = toSandbox "setenv" cache

getoptGenericsSandbox :: Path Sandbox
getoptGenericsSandbox = toSandbox "getopt-generics" cache
