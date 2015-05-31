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

import           Stack

spec :: Spec
spec =
  beforeAll_ unsetEnvVars $
  beforeAll mkCachedTestSandbox $ do

    describe "findPackageDB" $ do
      it "finds the sandbox package db" $ \ src -> do
        r <- findPackageDB src
        path r `shouldSatisfy` (\ pdb ->
          (path src </> ".cabal-sandbox") `isPrefixOf` pdb &&
          "ghc" `isInfixOf` pdb)

      it "returns an absolute path" $ \ src -> do
        r <- findPackageDB src
        path r `shouldSatisfy` ("/" `isPrefixOf`)

    describe "extractPackages" $ do
      it "extracts the packages" $ \ src -> do
        packageDB <- findPackageDB src
        packages <- extractPackages packageDB
        packages `shouldSatisfy` any (("tagged" `isInfixOf`) . path)
        packages `shouldSatisfy` all (("/" `isPrefixOf`) . path)

    describe "createStackedSandbox" $ do
      it "registers packages from one sandbox in another" $ \ src -> do
        inTempDirectoryNamed "b" $ do
          createStackedSandbox src
          output <- readProcess "cabal" (words "exec ghc-pkg list") ""
          output `shouldContain` target

      it "yields a working sandbox" $ \ src -> do
        withDirectory (path src) $ do
          hSilence [stderr] $ callCommand "cabal exec ghc-pkg check"

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
          listPackages >>= (`shouldNotContain` target)

unsetEnvVars :: IO ()
unsetEnvVars = do
  unsetEnv "CABAL_SANDBOX_CONFIG"
  unsetEnv "CABAL_SANDBOX_PACKAGE_PATH"
  unsetEnv "GHC_PACKAGE_PATH"

withDirectory :: FilePath -> IO a -> IO a
withDirectory dir action = bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
  createDirectoryIfMissing True dir
  setCurrentDirectory dir
  action

target :: String
target = "getopt-generics-0.6.3"

mkTestSandbox :: Path SandboxParent -> IO ()
mkTestSandbox dir = do
  withDirectory (path dir) $ do
    callCommand "cabal sandbox init"
    callCommand $ "cabal install " ++ target

mkCachedTestSandbox :: IO (Path SandboxParent)
mkCachedTestSandbox = do
  let relativeCache = "test-sandbox-cache"
  exists <- doesDirectoryExist relativeCache
  when (not exists) $ createDirectory relativeCache
  testSandboxCache <- Path <$> canonicalizePath relativeCache
  when (not exists) $ mkTestSandbox testSandboxCache
  return testSandboxCache
