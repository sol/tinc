{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Tinc.CacheSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Helper
import           MockedEnv
import           MockedProcess
import           Test.Mockery.Action

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.Graph.Wrapper as G
import           Safe
import           System.FilePath
import           System.IO.Temp
import           Test.Mockery.Directory

import           Tinc.Cache
import           Tinc.GhcPkg
import           Tinc.Package
import           Tinc.Sandbox hiding (recache)
import           Tinc.Types

data ReadGhcPkgEnv = ReadGhcPkgEnv {
  envReadGhcPkg :: [Path PackageDb] -> [String] -> IO String
}

ghcPkgEnv :: ReadGhcPkgEnv
ghcPkgEnv = ReadGhcPkgEnv readGhcPkg

instance GhcPkg (WithEnv ReadGhcPkgEnv) where
  readGhcPkg packageDbs args = WithEnv $ asks envReadGhcPkg >>= liftIO . ($ args) . ($ packageDbs)

spec :: Spec
spec = do
  describe "listPackageConfigs" $ do
    it "lists package configs in package database" $ withSystemTempDirectory "tinc" $ \ p -> do
      let packages = [
              CachedPackage (Package "foo" "2.1.7") (Path $ p </> "foo-2.1.7-8b77e2706d2c2c9243c5d86e44c11aa6.conf")
            , CachedPackage (Package "bar" "0.0.0") (Path $ p </> "bar-0.0.0-57c8091ea57afec62c051eda2322cc2f.conf")
            , CachedPackage (Package "baz" "0.6.1") (Path $ p </> "baz-0.6.1-91bc956c71d416cc2ca71cc535d34d6f.conf")
            ]
      mapM_ (touch . path . cachedPackageConfig) packages
      listPackageConfigs (Path p) >>= (`shouldMatchList` packages)

  describe "packageFromPackageConfig" $ do
    it "parses package from package config path" $ do
      packageFromPackageConfig "hspec-core-2.1.7-8b77e2706d2c2c9243c5d86e44c11aa6.conf" `shouldBe` Package "hspec-core" "2.1.7"

  describe "readPackageGraph" $ do
    context "when a package has no dependencies and no other packages depend on it" $ do
      it "includes package" $ do
        -- NOTE: `ghc-pkg dot` omits packages from the graph that both:
        --
        -- 1. have no dependencies
        -- 2. no other packages depend on
        --
        -- This test case makes sure that we properly handle this.

        withSystemTempDirectory "tinc" $ \ (Path -> packageDb) -> do
          let package = Package "foo" "0.1.0"
              packageConfig = Path $ path packageDb </> "foo-0.1.0-8b77e2706d2c2c9243c5d86e44c11aa6.conf"
              graph = "digraph g {}"
              globalPackageDb = "/path/to/global/package.conf.d"
              packageDbs = [globalPackageDb, packageDb]

              mockedEnv = ghcPkgEnv {envReadGhcPkg = mock (packageDbs, ["dot"], return graph)}
          touch $ path packageConfig

          withEnv mockedEnv (readPackageGraph [] globalPackageDb packageDb)
            `shouldReturn` G.fromList [(package, PackageConfig packageConfig, [])]

  describe "addAddSourceHashes" $ do
    let hash = "8cd0e753e18b1576cbe3eb2e61977a3b0debf430"
        foo = Package "foo" "0.1.0"
        writeAddSourceHashes packageDb =
          writeFile (path packageDb </> "add-source.yaml") "- {package-name: foo, hash: 8cd0e753e18b1576cbe3eb2e61977a3b0debf430}"

    it "adds add-source hashes to a package graph" $ do
      withSystemTempDirectory "tinc" $ \ (Path -> packageDb) -> do
        let fooConfig = PackageConfig ""
            graph = G.fromList [(foo, fooConfig, [])]
        writeAddSourceHashes packageDb
        addAddSourceHashes packageDb graph `shouldReturn`
          G.fromList [(setAddSourceHash hash foo, fooConfig, [])]

    it "doesn't attach add-source hashes to global packages" $ do
      withSystemTempDirectory "tinc" $ \ (Path -> packageDb) -> do
        let fooConfig = GlobalPackage
            graph = G.fromList [(foo, fooConfig, [])]
        writeAddSourceHashes packageDb
        addAddSourceHashes packageDb graph `shouldReturn`
          G.fromList [(foo, fooConfig, [])]

  describe "populateCache" $ do
    let mockedReadProcess = mockMany ([] :: [(String, [String], String, IO String)])
        cabalSandboxInit = ("cabal", ["sandbox", "init"], touch ".cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d/package.cache")

    it "uses add-source dependencies" $
      inTempDirectory $ do
        withSystemTempDirectory "tinc" $ \ (Path -> cache) -> do
          withSystemTempDirectory "tinc" $ \ (Path -> addSourceCache) -> do
            let mockedCallProcess command args = mockMany [cabalSandboxInit, cabalAddSource, cabalInstall, recache] command args
                  where
                    packageDb = atDef "/path/to/some/tmp/dir" args 3
                    cabalAddSource = ("cabal", ["sandbox", "add-source", path addSourceCache </> "foo" </> "abc"], writeFile "add-source" "foo")
                    cabalInstall = ("cabal", ["install", "foo-0.1.0"], (readFile "add-source" `shouldReturn` "foo") >> writeFile "install" "bar")
                    recache = ("ghc-pkg", ["--no-user-package-db", "recache", "--package-db", packageDb], return ())

                mockedEnv = env {envReadProcess = mockedReadProcess, envCallProcess = mockedCallProcess}
            _ <- withEnv mockedEnv $
              populateCache cache addSourceCache [Package "foo" "0.1.0"{versionAddSourceHash = Just "abc"}] []
            [sandbox] <- listSandboxes cache
            readFile (path sandbox </> "install") `shouldReturn` "bar"

    it "stores hashes of add-source dependencies in the cache" $
      inTempDirectory $ do
        withSystemTempDirectory "tinc" $ \ (Path -> cache) -> do
          withSystemTempDirectory "tinc" $ \ (Path -> addSourceCache) -> do
            let mockedCallProcess command args = mockMany [cabalSandboxInit, cabalAddSource "foo/abc", cabalAddSource "bar/def", cabalInstall, recache] command args
                  where
                    packageDb = atDef "/path/to/some/tmp/dir" args 3
                    cabalAddSource packageCachePath =
                      ("cabal", ["sandbox", "add-source", path addSourceCache </> packageCachePath], return ())
                    cabalInstall = ("cabal", ["install", "foo-0.1.0", "bar-0.1.0"], return ())
                    recache = ("ghc-pkg", ["--no-user-package-db", "recache", "--package-db", packageDb], return ())

                mockedEnv = env {envReadProcess = mockedReadProcess, envCallProcess = mockedCallProcess}
            let barPackageConfig = Path (path cache </> "foo")
            touch $ path barPackageConfig
            _ <- withEnv mockedEnv $
              populateCache cache addSourceCache
                [Package "foo" "0.1.0"{versionAddSourceHash = Just "abc"}]
                [CachedPackage (Package "bar" "0.1.0"{versionAddSourceHash = Just "def"}) barPackageConfig]
            [sandbox] <- listSandboxes cache
            packageDb <- findPackageDb sandbox
            readAddSourceHashes packageDb `shouldReturn` [AddSource "foo" "abc", AddSource "bar" "def"]

    context "when list of missing packages is empty" $ do
      it "returns reusable packages" $ do
        let mockedEnv = env {envReadProcess = undefined, envCallProcess = undefined}
            reusable = [
                CachedPackage (Package "foo" "0.1.0") "foo.conf"
              , CachedPackage (Package "bar" "0.1.0") "bar.conf"
              ]
        withEnv mockedEnv (populateCache undefined undefined [] reusable)
          `shouldReturn` reusable

  describe "listSandboxes" $ do
    it "lists sandboxes" $ do
      inTempDirectory $ do
        touch "foo/tinc.valid.v2"
        touch "bar/tinc.valid.v2"
        sandboxes <- listSandboxes "."
        sandboxes `shouldMatchList` ["./foo", "./bar"]

    it "rejects invalid sandboxes" $ do
      inTempDirectory $ do
        touch "foo/tinc.valid.v2"
        touch "bar/something"
        sandboxes <- listSandboxes "."
        sandboxes `shouldMatchList` ["./foo"]
