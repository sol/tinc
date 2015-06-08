{-# LANGUAGE ViewPatterns #-}

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
spec = beforeAll_ unsetEnvVars . beforeAll mkCache $ do
    describe "findPackageDB" $ do
      it "finds the sandbox package db" $ \ (toSandbox "getopt-generics" -> sandbox) -> do
        r <- findPackageDB sandbox
        path r `shouldSatisfy` (\ p -> (path sandbox </> ".cabal-sandbox") `isPrefixOf` p && isPackageDB p)

      it "returns an absolute path" $ \ (toSandbox "getopt-generics" -> sandbox) -> do
        r <- findPackageDB sandbox
        path r `shouldSatisfy` ("/" `isPrefixOf`)

    describe "extractPackages" $ do
      it "extracts the packages" $ \ (toSandbox "getopt-generics" -> sandbox) -> do
        packageDB <- findPackageDB sandbox
        packages <- extractPackages packageDB
        packages `shouldSatisfy` any (("tagged" `isInfixOf`) . path)
        packages `shouldSatisfy` all (("/" `isPrefixOf`) . path)

    describe "createStackedSandbox" $ do
      it "registers packages from one sandbox in another" $ \ (toSandbox "getopt-generics" -> sandbox) -> do
        inTempDirectory $ do
          createStackedSandbox sandbox
          output <- readProcess "cabal" (words "exec ghc-pkg list") ""
          output `shouldContain` showPackage getoptGenerics

      it "yields a working sandbox" $ \ (toSandbox "getopt-generics" -> sandbox) -> do
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
          installDependencies cache
          listPackages >>= (`shouldContain` "setenv")

      it "reuses packages" $ \ cache -> do
        inTempDirectoryNamed "foo" $ do
          writeFile "foo.cabal" . unlines $ cabalFile ++ ["    , setenv == 0.1.1.3"]
          installDependencies cache
          packageImportDirs "generics-sop" >>= (`shouldContain` path (toSandbox "getopt-generics" cache))
          packageImportDirs "setenv" >>= (`shouldContain` path (toSandbox "setenv" cache))

      it "skips redundant packages" $ \ cache -> do
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
withDirectory dir action = bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
  createDirectoryIfMissing True dir
  setCurrentDirectory dir
  action

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

mkCache :: IO (Path Cache)
mkCache = do
  createDirectoryIfMissing True "test-cache"
  cache <- Path <$> canonicalizePath "test-cache"
  mkTestSandbox cache "setenv" [Package "setenv" "0.1.1.3"]
  mkTestSandbox cache "getopt-generics" getoptGenericsPackages
  return cache

toSandbox :: String -> Path Cache -> Path Sandbox
toSandbox pattern (Path cache) = Path (cache </> "tinc-" ++ pattern)
