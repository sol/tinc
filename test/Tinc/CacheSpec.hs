{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Tinc.CacheSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Helper
import           MockedEnv
import           Test.Mockery.Action

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.Graph.Wrapper as G
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           Test.Mockery.Directory

import           Tinc.Cache
import           Tinc.GhcPkg
import           Tinc.Package
import           Tinc.Types

data Env = Env {
  envReadGhcPkg :: [Path PackageDb] -> [String] -> IO String
}

env :: Env
env = Env readGhcPkg

instance GhcPkg (WithEnv Env) where
  readGhcPkg packageDbs args = WithEnv $ asks envReadGhcPkg >>= liftIO . ($ args) . ($ packageDbs)

spec :: Spec
spec = do
  describe "listPackageConfigs" $ do
    it "lists package configs in package database" $ withSystemTempDirectory "tinc" $ \ p -> do
      let packages = [
              (Package "foo" "2.1.7", Path $ p </> "foo-2.1.7-8b77e2706d2c2c9243c5d86e44c11aa6.conf")
            , (Package "bar" "0.0.0", Path $ p </> "bar-0.0.0-57c8091ea57afec62c051eda2322cc2f.conf")
            , (Package "baz" "0.6.1", Path $ p </> "baz-0.6.1-91bc956c71d416cc2ca71cc535d34d6f.conf")
            ]
      mapM_ (touch . path . snd) packages
      listPackageConfigs (Path p) >>= (`shouldMatchList` packages)

  describe "packageFromPackageConfig" $ do
    it "parses package from package config path" $ do
      packageFromPackageConfig "hspec-core-2.1.7-8b77e2706d2c2c9243c5d86e44c11aa6.conf" `shouldBe` Package "hspec-core" "2.1.7"

  describe "findPackageDb" $ do
    it "finds the sandbox package db" $ do
      withSystemTempDirectory "tinc" $ \ sandbox -> do
        let packageDb = sandbox </> ".cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d"
        createDirectoryIfMissing True packageDb
        findPackageDb (Path sandbox) `shouldReturn` (Path packageDb)

    context "when sandbox does not contain a package db" $ do
      it "throws an exception" $ do
        withSystemTempDirectory "tinc" $ \ sandbox -> do
          let p = sandbox </> ".cabal-sandbox"
          createDirectory p
          findPackageDb (Path sandbox) `shouldThrow` errorCall ("src/Tinc/Cache.hs: No package database found in " ++ show p)

  describe "readPackageGraph" $ do
    context "when a package has no dependencies and no other packages depend on it" $ do
      it "includes package" $ do
        -- NOTE: `ghc-pkg dot` omits packages from the graph that both:
        --
        -- 1. have no dependencies
        -- 2. no other packages depend on
        --
        -- This test case makes sure that we properly handle this.

        let package = Package "foo" "0.1.0"
            graph = "digraph g {}"
            packageDbs = ["/path/to/package.conf.d"]

            mockedEnv = env {envReadGhcPkg = mock (packageDbs, ["dot"], return graph)}

        withEnv mockedEnv (readPackageGraph [(package, ())] packageDbs)
          `shouldReturn` G.fromList [(package, (), [])]
