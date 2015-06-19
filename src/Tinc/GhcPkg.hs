{-# LANGUAGE ViewPatterns #-}
module Tinc.GhcPkg (
  readGhcPkg
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
import           Tinc.PackageDb
import           Tinc.Types

listPackages :: [Path PackageDb] -> IO [Package]
listPackages packageDBs = parsePackages <$> readGhcPkg packageDBs ["list"]
  where
    parsePackages :: String -> [Package]
    parsePackages = map parsePackage . words

listGlobalPackages :: IO [Package]
listGlobalPackages = listPackages []

readPackageGraph :: [Path PackageDb] -> IO PackageGraph
readPackageGraph packageDBs = do
  dot <- readGhcPkg packageDBs ["dot"]
  packages <- listPackages packageDBs
  case fromDot packages dot of
    Right graph -> return graph
    Left message -> die message

readGhcPkg :: [Path PackageDb] -> [String] -> IO String
readGhcPkg (packageDBsToArgs -> packageDBs) args = do
  readProcess "ghc-pkg" ("--no-user-package-db" : "--simple-output" : packageDBs ++ args) ""

packageDBsToArgs :: [Path PackageDb] -> [String]
packageDBsToArgs packageDBs = concatMap (\ packageDB -> ["--package-db", path packageDB]) packageDBs
