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
    it "returns packageDb for specified path" $ withSystemTempDirectory "tinc" $ \ p -> do
      let packageDb = PackageDb (Map.fromList packageConfigs)
          packageConfigs = [
              (Package "foo" "2.1.7", Path $ p </> "foo-2.1.7-8b77e2706d2c2c9243c5d86e44c11aa6.conf")
            , (Package "bar" "0.0.0", Path $ p </> "bar-0.0.0-57c8091ea57afec62c051eda2322cc2f.conf")
            , (Package "baz" "0.6.1", Path $ p </> "baz-0.6.1-91bc956c71d416cc2ca71cc535d34d6f.conf")
            ]
      mapM_ (touch . path . snd) packageConfigs
      readPackageDb (Path p) `shouldReturn` packageDb

  describe "packageFromPackageConfig" $ do
    it "parses package from package config path" $ do
      packageFromPackageConfig "hspec-core-2.1.7-8b77e2706d2c2c9243c5d86e44c11aa6.conf" `shouldBe` Package "hspec-core" "2.1.7"
