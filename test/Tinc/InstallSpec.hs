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
import           Data.Version (makeVersion)
import           System.Directory hiding (withCurrentDirectory)
import           System.FilePath

import           Tinc.Facts
import           Tinc.Install
import           Tinc.Package
import           Tinc.AddSource
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

        mockedEnv :: (?mockedReadProcess :: ReadProcess, ?mockedCallProcess :: CallProcess) => Env
        mockedEnv = env {envReadProcess = ?mockedReadProcess, envCallProcess = ?mockedCallProcess}

        withMockedEnv :: (?mockedReadProcess :: ReadProcess, ?mockedCallProcess :: CallProcess) => WithEnv Env a -> IO a
        withMockedEnv = withEnv mockedEnv

    it "returns install plan" $ do
      withCabalFile $ \_ -> do
        let cabalInstallResult = return $ mkCabalInstallOutput ["setenv-0.1.1.3"]
        let ?mockedCallProcess = stub cabalSandboxInit
            ?mockedReadProcess = stub ("cabal", ["install", "--dry-run", "--only-dependencies", "--enable-tests"], "", cabalInstallResult)
        withMockedEnv (cabalInstallPlan facts [] []) `shouldReturn` [Package "setenv" "0.1.1.3"]

    it "takes add-source dependencies into account" $ do
      withCabalFile $ \sandbox -> do
        let name = "setenv"
            version = "0.1.1.2"
            hash = "fc2b9dbb754edcc14b0d9fa21201d67bc00794ec"
            cachedDependency = AddSource name hash
            addSourceCache = Path (sandbox </> "add-source-cache")
            dependency = Package name (Version version $ Just hash)

        dependencyPath <- createCachedAddSourceDependency addSourceCache cachedDependency version

        let cabalInstallResult = readFile "cabal-output"
        let ?mockedCallProcess = stub [
                cabalSandboxInit
              , ("cabal", ["sandbox", "add-source", dependencyPath], writeFile "cabal-output" $ mkCabalInstallOutput [showPackage dependency])
              ]
            ?mockedReadProcess = stub ("cabal", ["install", "--dry-run", "--only-dependencies", "--enable-tests", "--constraint=setenv == 0.1.0"], "", cabalInstallResult)
        withMockedEnv (cabalInstallPlan facts {factsAddSourceCache = addSourceCache} [] [(cachedDependency, makeVersion [0,1,0])]) `shouldReturn` [dependency]

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
            , "  other-modules:"
            , "      Paths_foo"
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
