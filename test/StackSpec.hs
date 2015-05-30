
module StackSpec where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad
import           Data.List.Compat
import           System.Directory
import           System.Environment.Compat
import           System.FilePath
import           System.IO
import           System.IO.Silently
import           System.Process
import           Test.Hspec
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
          output `shouldContain` "getopt-generics"

      it "yields a working sandbox" $ \ src -> do
        withDirectory (path src) $ do
          hSilence [stderr] $ callCommand "cabal exec ghc-pkg check"

unsetEnvVars :: IO ()
unsetEnvVars = do
  unsetEnv "CABAL_SANDBOX_CONFIG"
  unsetEnv "CABAL_SANDBOX_PACKAGE_PATH"
  unsetEnv "GHC_PACKAGE_PATH"

mkTestSandbox :: Path SandboxParent -> IO ()
mkTestSandbox dir = do
  withDirectory (path dir) $ do
    callCommand "cabal sandbox init"
    callCommand "cabal install getopt-generics"

mkCachedTestSandbox :: IO (Path SandboxParent)
mkCachedTestSandbox = do
  let relativeCache = "test-sandbox-cache"
  exists <- doesDirectoryExist relativeCache
  when (not exists) $ createDirectory relativeCache
  testSandboxCache <- Path <$> canonicalizePath relativeCache
  when (not exists) $ mkTestSandbox testSandboxCache
  return testSandboxCache
