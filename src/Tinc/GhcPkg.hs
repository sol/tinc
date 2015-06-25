{-# LANGUAGE ViewPatterns #-}
module Tinc.GhcPkg (
  PackageDb
, GhcPkg(..)
, listGlobalPackages
) where

import           Prelude ()
import           Prelude.Compat

import           System.Process

import           Tinc.Package
import           Tinc.Types

data PackageDb

class GhcPkg m where
  readGhcPkg :: [Path PackageDb] -> [String] -> m String

instance GhcPkg IO where
  readGhcPkg (packageDbsToArgs -> packageDbs) args = do
    readProcess "ghc-pkg" ("--no-user-package-db" : "--simple-output" : packageDbs ++ args) ""

listPackages :: [Path PackageDb] -> IO [Package]
listPackages packageDbs = parsePackages <$> readGhcPkg packageDbs ["list"]
  where
    parsePackages :: String -> [Package]
    parsePackages = map parsePackage . words

listGlobalPackages :: IO [Package]
listGlobalPackages = listPackages []

packageDbsToArgs :: [Path PackageDb] -> [String]
packageDbsToArgs packageDbs = concatMap (\ packageDb -> ["--package-db", path packageDb]) packageDbs
