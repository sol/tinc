{-# LANGUAGE OverloadedStrings #-}
module Tinc.PackageDbSpec where

import qualified Data.Map as Map
import           System.FilePath
import           System.IO.Temp
import           Test.Hspec
import           Test.Mockery.Directory

import           Package
import           Tinc.PackageDb
import           Tinc.Types

spec :: Spec
spec = do
  describe "readPackageDb" $ do
    it "returns packageDb for specified path" $ withSystemTempDirectory "tinc" $ \ path -> do
      let packageDb = PackageDb (Path path) (Map.fromList packageConfigs)
          packageConfigs = [
              (Package "hspec" "2.1.7", Path $ path </> "hspec-2.1.7-8b77e2706d2c2c9243c5d86e44c11aa6.conf")
            --, (Package "tinc" "0.0.0", Path $ path </> "tinc-0.0.0-57c8091ea57afec62c051eda2322cc2f.conf")
            --, (Package "hpack" "0.6.1.1", Path $ path </> "hpack-0.6.1.1-91bc956c71d416cc2ca71cc535d34d6f.conf")
            ]
      touch $ path </> "hspec-2.1.7-8b77e2706d2c2c9243c5d86e44c11aa6.conf"
      --touch $ path </> "tinc-0.0.0-57c8091ea57afec62c051eda2322cc2f.conf"
      --touch $ path </> "hpack-0.6.1.1-91bc956c71d416cc2ca71cc535d34d6f.conf"

      readPackageDb (Path path) `shouldReturn` packageDb

  describe "lookupPackageConfig" $ do
    let package = Package "foo" "0.1.0"
        packageConfig = "/path/to/package-db/foo-0.1.0-41a20dbac2e8ab6d81306cb757375b6b.conf"
    it "returns package config for specified package" $ do
      let packageDb = PackageDb "/path/to/package-db/" (Map.fromList [(package, packageConfig)])
      lookupPackageConfig packageDb package `shouldReturn` packageConfig

    context "when package config is not found" $ do
      it "throws an exception" $ do
        let packageDb = PackageDb "/path/to/package-db/" Map.empty
        lookupPackageConfig packageDb package `shouldThrow` errorCall "no package config found for foo-0.1.0 in /path/to/package-db/"

  describe "packageFromPackageConfig" $ do
    it "parses package from package config path" $ do
      packageFromPackageConfig "hspec-core-2.1.7-8b77e2706d2c2c9243c5d86e44c11aa6.conf" `shouldBe` Package "hspec-core" "2.1.7"
