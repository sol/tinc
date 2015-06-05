module StackSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad
import           Control.Exception
import           Data.List.Compat
import           System.Directory
import           System.Environment.Compat
import           System.FilePath
import           System.IO
import           System.IO.Silently
import           System.Process
import           Test.Hspec
import           Test.Hspec.Expectations.Contrib
import           Test.Mockery.Directory

import           Util
import           Stack

spec :: Spec
spec = beforeAll_ unsetEnvVars . beforeAll mkCachedTestSandbox $ do
    describe "findPackageDB" $ do
      it "finds the sandbox package db" $ \ sandbox -> do
        r <- findPackageDB sandbox
        path r `shouldSatisfy` (\ p -> (path sandbox </> ".cabal-sandbox") `isPrefixOf` p && isPackageDB p)

      it "returns an absolute path" $ \ sandbox -> do
        r <- findPackageDB sandbox
        path r `shouldSatisfy` ("/" `isPrefixOf`)

    describe "extractPackages" $ do
      it "extracts the packages" $ \ sandbox -> do
        packageDB <- findPackageDB sandbox
        packages <- extractPackages packageDB
        packages `shouldSatisfy` any (("tagged" `isInfixOf`) . path)
        packages `shouldSatisfy` all (("/" `isPrefixOf`) . path)

    describe "createStackedSandbox" $ do
      it "registers packages from one sandbox in another" $ \ sandbox -> do
        inTempDirectory $ do
          createStackedSandbox sandbox
          output <- readProcess "cabal" (words "exec ghc-pkg list") ""
          output `shouldContain` getoptGenerics

      it "yields a working sandbox" $ \ sandbox -> do
        inTempDirectory $ do
          createStackedSandbox sandbox
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

      it "installs dependencies" $ \ cache -> do
        inTempDirectoryNamed "foo" $ do
          writeFile "foo.cabal" . unlines $ cabalFile ++ ["    , setenv"]
          installDependencies cache
          listPackages >>= (`shouldContain` "setenv")

      it "reuses packages" $ \ cache -> do
        inTempDirectoryNamed "foo" $ do
          writeFile "foo.cabal" $ unlines cabalFile
          installDependencies cache
          packageImportDirs "generics-sop" >>= (`shouldContain` path cache)

      it "skips redundant packages" $ \ cache -> do
        inTempDirectoryNamed "foo" $ do
          writeFile "foo.cabal" $ unlines cabalFile
          installDependencies cache
          listPackages >>= (`shouldNotContain` getoptGenerics)

unsetEnvVars :: IO ()
unsetEnvVars = do
  unsetEnv "CABAL_SANDBOX_CONFIG"
  unsetEnv "CABAL_SANDBOX_PACKAGE_PATH"
  unsetEnv "GHC_PACKAGE_PATH"

withDirectory :: FilePath -> IO a -> IO a
withDirectory dir action = bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
  createDirectoryIfMissing True dir
  setCurrentDirectory dir
  action

getoptGenerics :: String
getoptGenerics = "getopt-generics-0.6.3"

ghcPkgCheck :: IO ()
ghcPkgCheck = hSilence [stderr] $ callCommand "cabal exec ghc-pkg check"

mkTestSandbox :: Path Sandbox -> IO ()
mkTestSandbox dir = do
  withDirectory (path dir) $ do
    callCommand "cabal sandbox init"
    callCommand ("cabal install --disable-library-profiling --disable-optimization --disable-documentation " ++ unwords packages)
  where
    packages = [
        "base-compat-0.8.2"
      , "base-orphans-0.3.2"
      , "generics-sop-0.1.1.2"
      , "tagged-0.8.0.1"
      , getoptGenerics
      ]

cacheDir :: FilePath
cacheDir = "test-cache/tinc-1209"

mkCachedTestSandbox :: IO (Path Sandbox)
mkCachedTestSandbox = do
  exists <- doesDirectoryExist cacheDir
  when (not exists) $ createDirectoryIfMissing True cacheDir
  sandbox <- Path <$> canonicalizePath cacheDir
  when (not exists) $ mkTestSandbox sandbox
  return sandbox
