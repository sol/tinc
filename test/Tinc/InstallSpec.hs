{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Tinc.InstallSpec (spec) where

import           Helper
import           MockedEnv
import           MockedProcess
import           Test.Mockery.Action

import           Data.List
import           System.Directory
import           System.FilePath

import           Tinc.Install
import           Tinc.Package
import           Tinc.Sandbox
import           Tinc.Types
import           Util

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

createCachedAddSourceDependency :: Path AddSourceCache -> AddSource -> String -> IO FilePath
createCachedAddSourceDependency addSourceCache AddSource{..} version = do
  createDirectoryIfMissing True dependencyPath
  withCurrentDirectory dependencyPath $ do
    writeCabalFile addSourcePackageName version []
  return dependencyPath
  where
    dependencyPath = path addSourceCache </> addSourcePackageName </> addSourceHash

spec :: Spec
spec = do
  describe "cabalInstallPlan" $ do

    let cabalSandboxInit = ("cabal", ["sandbox", "init"], touch ".cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d/package.cache")

        withCabalFile action = inTempDirectory $ do
          writeCabalFile "foo" "0.0.0" ["setenv <= 0.1.1.3"]
          getCurrentDirectory >>= action

        mkCabalInstallOutput :: [String] -> String
        mkCabalInstallOutput dependencies = unlines $ [
            "Resolving dependencies..."
          , "In order, the following would be installed (use -v for more details):"
          ] ++ dependencies

        mockedEnv :: (?cabalInstallResult :: IO String, ?mockedCallProcess :: CallProcess) => Env
        mockedEnv = env {envReadProcess = mockedReadProcess, envCallProcess = ?mockedCallProcess}
          where
            mockedReadProcess = stub ("cabal", ["--remote-repo-cache", "/some/path/to/remote-repo-cache", "install", "--dry-run", "--only-dependencies", "--enable-tests"], "", ?cabalInstallResult)

        withMockedEnv :: (?cabalInstallResult :: IO String, ?mockedCallProcess :: CallProcess) => WithEnv Env a -> IO a
        withMockedEnv = withEnv mockedEnv

    it "returns install plan" $ do
      withCabalFile $ \_ -> do
        let ?cabalInstallResult = return $ mkCabalInstallOutput ["setenv-0.1.1.3"]
            ?mockedCallProcess = stub cabalSandboxInit
        withMockedEnv (cabalInstallPlan facts [] undefined []) `shouldReturn` [Package "setenv" "0.1.1.3"]

    it "takes add-source dependencies into account" $ do
      withCabalFile $ \sandbox -> do
        let name = "setenv"
            version = "0.1.1.2"
            hash = "fc2b9dbb754edcc14b0d9fa21201d67bc00794ec"
            cachedDependency = AddSource name hash
            addSourceCache = Path (sandbox </> "add-source-cache")
            dependency = Package name (Version version $ Just hash)

        dependencyPath <- createCachedAddSourceDependency addSourceCache cachedDependency version

        let ?cabalInstallResult = readFile "cabal-output"
            ?mockedCallProcess = stub [
                cabalSandboxInit
              , ("cabal", ["sandbox", "add-source", dependencyPath], writeFile "cabal-output" $ mkCabalInstallOutput [showPackage dependency])
              ]
        withMockedEnv (cabalInstallPlan facts [] addSourceCache [cachedDependency]) `shouldReturn` [dependency]

  describe "copyFreezeFile" $ do
    it "copies freeze file" $ do
      inTempDirectory $ do
        writeFile "cabal.config" "some constraints"
        touch "foo/bar"
        copyFreezeFile "foo"
        readFile "foo/cabal.config" `shouldReturn` "some constraints"

    context "when there is no freeze file" $ do
      it "does nothing" $ do
        inTempDirectory $ do
          copyFreezeFile "foo"

  describe "generateCabalFile" $ do
    context "when there are additional dependencies" $ do
      it "generates a cabal file" $ do
        inTempDirectory $ do
          generateCabalFile ["foo"] `shouldReturn` ("tinc-generated.cabal", unlines [
              "name: tinc-generated"
            , "version: 0.0.0"
            , "build-type: Simple"
            , "cabal-version: >= 1.10"
            , ""
            , "executable tinc-generated"
            , "  main-is: Generated.hs"
            , "  build-depends:"
            , "      foo"
            , "  default-language: Haskell2010"
            ])

    context "when there is a package.yaml" $ do
      it "generates a cabal file" $ do
        inTempDirectory $ do
          writeFile "package.yaml" $ unlines [
              "name: foo"
            ]
          generateCabalFile [] `shouldReturn` ("foo.cabal", unlines [
              "name: foo"
            , "version: 0.0.0"
            , "build-type: Simple"
            , "cabal-version: >= 1.10"
            ])

    context "when there are both a package.yaml and additional dependencies" $ do
      it "combines them" $ do
        inTempDirectory $ do
          writeFile "package.yaml" $ unlines [
              "name: foo"
            , "library:"
            , "  dependencies: foo"
            ]
          generateCabalFile ["bar"] `shouldReturn` ("tinc-generated.cabal", unlines [
              "name: tinc-generated"
            , "version: 0.0.0"
            , "build-type: Simple"
            , "cabal-version: >= 1.10"
            , ""
            , "library"
            , "  build-depends:"
            , "      foo"
            , "  default-language: Haskell2010"
            , ""
            , "executable tinc-generated"
            , "  main-is: Generated.hs"
            , "  build-depends:"
            , "      bar"
            , "  default-language: Haskell2010"
            ])

    context "when there is a cabal file" $ do
      it "returns contents" $ do
        inTempDirectory $ do
          writeFile "foo.cabal" "foo"
          generateCabalFile [] `shouldReturn` ("foo.cabal", "foo")

      context "when there are additional dependencies" $ do
        it "ignores them (for now)" $ do
          inTempDirectory $ do
            writeFile "foo.cabal" "foo"
            generateCabalFile ["foo"] `shouldReturn` ("foo.cabal", "foo")

    context "when there are multiple cabal files" $ do
      it "fails" $ do
        inTempDirectory $ do
          touch "foo.cabal"
          touch "bar.cabal"
          generateCabalFile [] `shouldThrow` errorCall "Multiple cabal files found."

    context "when there is no cabal file" $ do
      it "fails" $ do
        inTempDirectory $ do
          generateCabalFile [] `shouldThrow` errorCall "No cabal file found."

  describe "cloneRemoteRepoCache" $ do
    let
      remote = "some-remote"
      tarFile = "00-index.tar"
      cacheFile = "00-index.cache"
      cacheFileContents = "index cache\n"
      setup = do
        touch ("src" </> remote </> tarFile)
        writeFile ("src" </> remote </> cacheFile) cacheFileContents

    around_ inTempDirectory $ before_ setup $ do
      it "symlinks tar file" $ do
        cloneRemoteRepoCache [] "src" "dst"
        srcTarFile <- canonicalizePath ("src" </> remote </> tarFile)
        canonicalizePath ("dst" </> remote </> tarFile) `shouldReturn` srcTarFile

      it "copies cache file" $ do
        cloneRemoteRepoCache [] "src" "dst"
        readFile ("dst" </> remote </> cacheFile) `shouldReturn` cacheFileContents

      it "removes add-source dependencies from cache file" $ do
        let
          cacheFileLines = [
              "pkg: foo 0.1.0 b# 240236"
            , "pkg: bar 0.1.0 b# 240240"
            , "pkg: bar 0.2.0 b# 240242"
            ]
        writeFile ("src" </> remote </> cacheFile) $ unlines cacheFileLines
        cloneRemoteRepoCache [AddSource "bar" "some-hash"] "src" "dst"
        readFile ("dst" </> remote </> cacheFile) `shouldReturn` unlines (take 1 cacheFileLines)

    describe "removeAddSourceDependencies" $ do
      it "removes add-source dependencies" $ do
        let input = ["pkg: foo 0.1.0 b# 240236"]
        removeAddSourceDependencies [AddSource "foo" "some-hash"] input `shouldBe` []

      it "keeps other dependencies" $ do
        let input = ["pkg: foo-bar 0.1.0 b# 240236"]
        removeAddSourceDependencies [AddSource "foo" "some-hash"] input `shouldBe` input

      it "keeps preferred versions" $ do
        let input = ["pref-ver: foo >0.1.0.0"]
        removeAddSourceDependencies [AddSource "foo" "some-hash"] input `shouldBe` input

    describe "listRemoteRepos" $ around_ inTempDirectory $ do
      it "list remotes" $ do
        touch "cache/foo/00-index.tar"
        listRemoteRepos "cache" `shouldReturn` ["foo"]

      it "ignores regular files" $ do
        touch "cache/foo"
        listRemoteRepos "cache" `shouldReturn` []

      it "ignores other directories" $ do
        touch "cache/foo/.placeholder"
        listRemoteRepos "cache" `shouldReturn` []
