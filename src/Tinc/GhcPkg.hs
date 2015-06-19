{-# LANGUAGE ViewPatterns #-}
module Tinc.GhcPkg (
  PackageDb
, readGhcPkg
, listPackages
, listGlobalPackages
, readPackageGraph
) where

import           Prelude ()
import           Prelude.Compat

import           System.Exit.Compat
import           System.Process

import           Package
import           PackageGraph
import           Tinc.Types
import           Tinc.GhcInfo

listPackages :: [Path PackageDb] -> IO [Package]
listPackages packageDbs = parsePackages <$> readGhcPkg packageDbs ["list"]
  where
    parsePackages :: String -> [Package]
    parsePackages = map parsePackage . words

listGlobalPackages :: IO [Package]
listGlobalPackages = listPackages []

readPackageGraph :: [Path PackageDb] -> IO PackageGraph
readPackageGraph packageDbs = do
  dot <- readGhcPkg packageDbs ["dot"]
  packages <- listPackages packageDbs
  case fromDot packages dot of
    Right graph -> return graph
    Left message -> die message

readGhcPkg :: [Path PackageDb] -> [String] -> IO String
readGhcPkg (packageDbsToArgs -> packageDbs) args = do
  readProcess "ghc-pkg" ("--no-user-package-db" : "--simple-output" : packageDbs ++ args) ""

packageDbsToArgs :: [Path PackageDb] -> [String]
packageDbsToArgs packageDbs = concatMap (\ packageDb -> ["--package-db", path packageDb]) packageDbs
