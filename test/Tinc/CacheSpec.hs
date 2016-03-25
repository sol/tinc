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
import           Data.List
import qualified Data.Graph.Wrapper as G
import           Safe
import           System.FilePath
import           System.IO.Temp
import           System.Directory
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

              mockedEnv = ghcPkgEnv {envReadGhcPkg = stub (packageDbs, ["dot"], return graph)}
          touch $ path packageConfig
          touch $ path packageDb </> "package.cache"

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

  describe "populateCacheAction" $ do
    let addSourceCache = "/path/to/add-source-cache"

    it "adds add-source dependencies to the sandbox" $ do
      let missing = [Package "foo" (Version "0.1.0" $ Just "foo-hash")]
      populateCacheActionAddSource <$> populateCacheAction addSourceCache missing [] `shouldBe`
        Right ["/path/to/add-source-cache/foo/foo-hash"]

    it "does not add reusable add-source dependencies to the sandbox" $ do
      let missing = [Package "foo" "0.1.0"]
          reusable = [CachedPackage (Package "bar" (Version "0.2.0" $ Just "bar-hash")) "bar.conf"]
      populateCacheActionAddSource <$> populateCacheAction addSourceCache missing reusable `shouldBe` Right []

    it "does not include reusable add-source dependencies in the install plan" $ do
      let missing = [Package "foo" "0.1.0"]
          reusable = [CachedPackage (Package "bar" (Version "0.2.0" $ Just "bar-hash")) "bar.conf"]
      populateCacheActionInstallPlan <$> populateCacheAction addSourceCache missing reusable `shouldBe` Right missing

    it "stores hashes of add-source dependencies in the cache" $ do
      let missing = [Package "foo" (Version "0.1.0" $ Just "foo-hash")]
          reusable = [CachedPackage (Package "bar" (Version "0.2.0" $ Just "bar-hash")) "bar.conf"]
      populateCacheActionWriteAddSourceHashes <$> populateCacheAction addSourceCache missing reusable `shouldBe`
        Right [AddSource "foo" "foo-hash", AddSource "bar" "bar-hash"]

    context "when list of missing packages is empty" $ do
      let missing = []
      it "returns reusable packages" $ do
        let reusable = [CachedPackage (Package "foo" "0.1.0") "foo.conf", CachedPackage (Package "bar" "0.2.0") "bar.conf"]
        populateCacheAction addSourceCache missing reusable `shouldBe` Left reusable

  describe "populateCache" $ do
    let cabalSandboxInit = ("cabal", ["sandbox", "init"], touch ".cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d/package.cache")

    it "uses add-source dependencies" $
      inTempDirectory $ do
        withSystemTempDirectory "tinc" $ \ (Path -> cache) -> do
          withSystemTempDirectory "tinc" $ \ (Path -> addSourceCache) -> do
            let mockedCallProcess command args = stubMany [cabalSandboxInit, cabalAddSource, cabalInstall, recache] command args
                  where
                    packageDb = atDef "/path/to/some/tmp/dir" args 3
                    cabalAddSource = ("cabal", ["sandbox", "add-source", path addSourceCache </> "foo" </> "abc"], writeFile "add-source" "foo")
                    cabalInstall = ("cabal", ["install", "--bindir=$prefix/bin/$pkgid", "foo-0.1.0"], (readFile "add-source" `shouldReturn` "foo") >> writeFile "install" "bar")
                    recache = ("ghc-pkg", ["--no-user-package-db", "recache", "--package-db", packageDb], return ())

                mockedEnv = env {envReadProcess = dummy "envReadProcess", envCallProcess = mockedCallProcess}
            _ <- withEnv mockedEnv $
              populateCache cache addSourceCache [Package "foo" "0.1.0"{versionAddSourceHash = Just "abc"}] []
            [sandbox] <- listSandboxes cache
            readFile (path sandbox </> "install") `shouldReturn` "bar"

    it "stores hashes of add-source dependencies in the cache" $
      inTempDirectory $ do
        withSystemTempDirectory "tinc" $ \ (Path -> cache) -> do
          withSystemTempDirectory "tinc" $ \ (Path -> addSourceCache) -> do
            let mockedCallProcess command args = stubMany [cabalSandboxInit, cabalAddSource "foo/abc", cabalAddSource "bar/def", cabalInstall, recache] command args
                  where
                    packageDb = atDef "/path/to/some/tmp/dir" args 3
                    cabalAddSource packageCachePath =
                      ("cabal", ["sandbox", "add-source", path addSourceCache </> packageCachePath], return ())
                    cabalInstall = ("cabal", ["install", "--bindir=$prefix/bin/$pkgid", "foo-0.1.0"], return ())
                    recache = ("ghc-pkg", ["--no-user-package-db", "recache", "--package-db", packageDb], return ())

                mockedEnv = env {envReadProcess = dummy "envReadProcess", envCallProcess = mockedCallProcess}
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
        touch "foo/tinc.valid.v3"
        touch "bar/tinc.valid.v3"
        sandboxes <- listSandboxes "."
        sandboxes `shouldMatchList` ["./foo", "./bar"]

    it "rejects invalid sandboxes" $ do
      inTempDirectory $ do
        touch "foo/tinc.valid.v3"
        touch "bar/something"
        sandboxes <- listSandboxes "."
        sandboxes `shouldMatchList` ["./foo"]

  describe "cachedExecutables" $ do
    let sandbox = ".cabal-sandbox"
        packageConfig = Path (sandbox </> "packages.conf.d/markdown-unlit-0.1.0-269c14.conf")
        package = Package "markdown-unlit" "0.1.0"
        cachedPackage = CachedPackage package packageConfig
        executables = [
            sandbox </> "bin/markdown-unlit-0.1.0/foo"
          , sandbox </> "bin/markdown-unlit-0.1.0/bar"
          ]
    it "returns executables for specified package" $ do
      inTempDirectory $ do
        touch (path packageConfig)
        mapM_ touch executables
        dir <- getCurrentDirectory
        cachedExecutables cachedPackage `shouldReturn` sort (map (dir </>) executables)

    context "when package has no executables" $ do
      it "returns empty list" $ do
        inTempDirectory $ do
          touch (path packageConfig)
          cachedExecutables cachedPackage `shouldReturn` []
