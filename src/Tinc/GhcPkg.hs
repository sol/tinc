{-# LANGUAGE ViewPatterns #-}
module Tinc.GhcPkg (
  PackageDB
, readGhcPkg
, listPackages
, listGlobalPackages
) where

import           Prelude ()
import           Prelude.Compat

import           System.Process

import           Package
import           Tinc.Types
import           Tinc.GhcInfo

listPackages :: [Path PackageDB] -> IO [Package]
listPackages packageDBs = parsePackages <$> readGhcPkg packageDBs ["list"]
  where
    parsePackages :: String -> [Package]
    parsePackages = map parsePackage . words

listGlobalPackages :: IO [Package]
listGlobalPackages = listPackages []

readGhcPkg :: [Path PackageDB] -> [String] -> IO String
readGhcPkg (packageDBsToArgs -> packageDBs) args = do
  readProcess "ghc-pkg" ("--no-user-package-db" : "--simple-output" : packageDBs ++ args) ""

packageDBsToArgs :: [Path PackageDB] -> [String]
packageDBsToArgs packageDBs = concatMap (\ packageDB -> ["--package-db", path packageDB]) packageDBs
