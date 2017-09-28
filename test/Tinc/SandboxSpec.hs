{-# LANGUAGE OverloadedStrings #-}
module Tinc.SandboxSpec where

import Helper

import           System.Directory
import           System.FilePath
import           System.IO.Temp

import           Util
import           Tinc.Package
import           Tinc.Sandbox
import           Tinc.Types

writePackageConfig :: (Package, FilePath) -> IO ()
writePackageConfig (Package name (Version version _), packageConfig) = do
  writeFile packageConfig $ unlines [
      "name: " ++ name
    , "version: " ++ version
    ]

spec :: Spec
spec = do
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
          findPackageDb (Path sandbox) `shouldThrow` errorCall ("src/Tinc/Sandbox.hs: No package database found in " ++ show p)

  describe "listPackages" $ do
    it "lists packages package database" $ withSystemTempDirectory "tinc" $ \ p -> do
      let packages = [
              (Package "foo" "2.1.7", p </> "foo-2.1.7-8b77e2706d2c2c9243c5d86e44c11aa6.conf")
            , (Package "bar" "0.0.0", p </> "bar-0.0.0-57c8091ea57afec62c051eda2322cc2f.conf")
            , (Package "baz" "0.6.1", p </> "baz-0.6.1-91bc956c71d416cc2ca71cc535d34d6f.conf")
            ]
      mapM_ writePackageConfig packages
      listPackages (Path p) >>= (`shouldMatchList` packages)

    it "returns canonical file paths to package configs" $ do
      -- NOTE: This behavior is crucial for executable caching to work properly
      -- as the executables are found relative to the canonical location of the
      -- package config!

      inTempDirectory $ do
        let packageDb = ".cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d"
            packageConfig = "foo" </> "foo-0.1.0-8b77e2.conf"
            package = Package "foo" "0.1.0"

        createDirectoryIfMissing True (path packageDb)
        createDirectoryIfMissing False "bar"
        createDirectoryIfMissing False "foo"
        writePackageConfig (package, packageConfig)

        linkFile packageConfig "bar"
        registerPackage packageDb (Path $ "bar" </> takeFileName packageConfig)

        dir <- getCurrentDirectory
        listPackages packageDb `shouldReturn` [(package, dir </> packageConfig)]

  describe "packageFromPackageConfig" $ do
    it "parses package from package config path" $ do
      inTempDirectory $ do
        let conf = "my.conf"
        writeFile conf $ unlines [
            "name: tinc"
          , "version: 0.1.0"
          ]
        packageFromPackageConfig conf `shouldReturn` Package "tinc" "0.1.0"
