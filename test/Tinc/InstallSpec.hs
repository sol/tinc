{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImplicitParams #-}
module Tinc.InstallSpec (spec) where

import           Helper
import           MockedEnv
import           MockedProcess
import           Test.Mockery.Action

import           Data.List
import           System.Directory
import           System.FilePath
import           Test.Mockery.Directory

import           Util
import           Tinc.Git
import           Tinc.Install
import           Tinc.Package
import           Tinc.Types

writeCabalFile :: String -> String -> [String] -> IO ()
writeCabalFile name version dependencies = do
  writeFile (name ++ ".cabal") $ unlines [
      "name: " ++ name
    , "version: " ++ version
    , "build-type: Simple"
    , "cabal-version: >= 1.10"
    , "library"
    , "  build-depends: " ++ intercalate ", " dependencies
    ]

createCachedGitDependency :: Path GitCache -> CachedGitDependency -> String -> IO FilePath
createCachedGitDependency gitCache CachedGitDependency{..} version = do
  createDirectoryIfMissing True gitDependencyPath
  withCurrentDirectory gitDependencyPath $ do
    writeCabalFile cachedGitDependencyName version []
  return gitDependencyPath
  where
    gitDependencyPath = path gitCache </> cachedGitDependencyName </> cachedGitDependencyRevision

spec :: Spec
spec = do
  let cabalSandboxInit = ("cabal", ["sandbox", "init"], touch ".cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d/package.cache")

  describe "cabalInstallPlan" $ do
    let withCabalFile action = inTempDirectory $ do
          writeCabalFile "foo" "0.0.0" ["setenv <= 0.1.1.3"]
          getCurrentDirectory >>= action

        mkCabalInstallOutput :: [String] -> String
        mkCabalInstallOutput dependencies = unlines $ [
            "Resolving dependencies..."
          , "In order, the following would be installed (use -v for more details):"
          ] ++ dependencies

        mockedEnv :: (?target :: FilePath, ?cabalInstallResult :: IO String, ?mockedCallProcess :: CallProcess) => Env
        mockedEnv = env {envReadProcess = mockedReadProcess, envCallProcess = ?mockedCallProcess}
          where
            mockedReadProcess = mock ("cabal", ["install", "--only-dependencies", "--enable-tests", "--dry-run", ?target], "", ?cabalInstallResult)

        withMockedEnv :: (?target :: FilePath, ?cabalInstallResult :: IO String, ?mockedCallProcess :: CallProcess) => WithEnv Env a -> IO a
        withMockedEnv = withEnv mockedEnv

    it "returns install plan" $ do
      withCabalFile $ \sandbox -> do
        let ?target = sandbox
            ?cabalInstallResult = return $ mkCabalInstallOutput ["setenv-0.1.1.3"]
            ?mockedCallProcess = mock cabalSandboxInit
        withMockedEnv (cabalInstallPlan undefined []) `shouldReturn` [Package "setenv" "0.1.1.3"]

    it "takes git dependencies into account" $ do
      withCabalFile $ \sandbox -> do
        let name = "setenv"
            version = "0.1.1.2"
            revision = "fc2b9dbb754edcc14b0d9fa21201d67bc00794ec"
            cachedGitDependency = CachedGitDependency name revision
            gitCache = Path (sandbox </> "git-cache")
            gitDependency = Package name (Version version $ Just revision)

        gitDependencyPath <- createCachedGitDependency gitCache cachedGitDependency version

        let ?target = sandbox
            ?cabalInstallResult = readFile "cabal-output"
            ?mockedCallProcess = mockMany [
                cabalSandboxInit
              , ("cabal", ["sandbox", "add-source", gitDependencyPath], writeFile "cabal-output" $ mkCabalInstallOutput [showPackage gitDependency])
              ]
        withMockedEnv (cabalInstallPlan gitCache [cachedGitDependency]) `shouldReturn` [gitDependency]
