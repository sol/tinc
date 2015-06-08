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

import           Package
import           Stack

spec :: Spec
spec = beforeAll_ unsetEnvVars . beforeAll (mkCachedTestSandbox "getopt-generics" getoptGenericsPackages) $ do
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
          output `shouldContain` showPackage getoptGenerics

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
          writeFile "foo.cabal" . unlines $ cabalFile ++ ["    , setenv == 0.1.1.3"]
          installDependencies [cache]
          listPackages >>= (`shouldContain` "setenv")

      it "reuses packages" $ \ cache -> do
        inTempDirectoryNamed "foo" $ do
          setenvCache <- mkCachedTestSandbox "setenv" [Package "setenv" "0.1.1.3"]
          writeFile "foo.cabal" . unlines $ cabalFile ++ ["    , setenv == 0.1.1.3"]
          installDependencies [cache, setenvCache]
          packageImportDirs "generics-sop" >>= (`shouldContain` path cache)
          packageImportDirs "setenv" >>= (`shouldContain` path setenvCache)

      it "skips redundant packages" $ \ cache -> do
        inTempDirectoryNamed "foo" $ do
          writeFile "foo.cabal" $ unlines cabalFile
          installDependencies [cache]
          listPackages >>= (`shouldNotContain` showPackage getoptGenerics)

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

getoptGenerics :: Package
getoptGenerics = Package "getopt-generics" "0.6.3"

ghcPkgCheck :: IO ()
ghcPkgCheck = hSilence [stderr] $ callCommand "cabal exec ghc-pkg check"

mkTestSandbox :: [Package] -> Path Sandbox -> IO ()
mkTestSandbox packages dir = do
  withDirectory (path dir) $ do
    callCommand "cabal sandbox init"
    callCommand ("cabal install --disable-library-profiling --disable-optimization --disable-documentation " ++
                 unwords (map showPackage packages))

getoptGenericsPackages :: [Package]
getoptGenericsPackages = [
    Package "base-compat" "0.8.2"
  , Package "base-orphans" "0.3.2"
  , Package "generics-sop" "0.1.1.2"
  , Package "tagged" "0.8.0.1"
  , getoptGenerics
  ]

mkCachedTestSandbox :: String -> [Package] -> IO (Path Sandbox)
mkCachedTestSandbox pattern packages = do
  exists <- doesDirectoryExist cacheDir
  when (not exists) $ createDirectoryIfMissing True cacheDir
  sandbox <- Path <$> canonicalizePath cacheDir
  when (not exists) $ mkTestSandbox packages sandbox
  return sandbox
  where
    cacheDir :: FilePath
    cacheDir = "test-cache/tinc-" ++ pattern
