{-# LANGUAGE ViewPatterns #-}
module Tinc.GhcPkg (
  PackageDb
, readGhcPkg
, listGlobalPackages
) where

import           Prelude ()
import           Prelude.Compat

import           System.Process

import           Tinc.Package
import           Tinc.Types

data PackageDb

listPackages :: [Path PackageDb] -> IO [Package]
listPackages packageDbs = parsePackages <$> readGhcPkg packageDbs ["list"]
  where
    parsePackages :: String -> [Package]
    parsePackages = map parsePackage . words

listGlobalPackages :: IO [Package]
listGlobalPackages = listPackages []

readGhcPkg :: [Path PackageDb] -> [String] -> IO String
readGhcPkg (packageDbsToArgs -> packageDbs) args = do
  readProcess "ghc-pkg" ("--no-user-package-db" : "--simple-output" : packageDbs ++ args) ""

packageDbsToArgs :: [Path PackageDb] -> [String]
packageDbsToArgs packageDbs = concatMap (\ packageDb -> ["--package-db", path packageDb]) packageDbs
