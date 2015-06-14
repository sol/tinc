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

data PackageDB

listPackages :: Path PackageDB -> IO [Package]
listPackages (Path packageDB) = parsePackages <$> readGhcPkg ["--package-db", packageDB, "list"]

listGlobalPackages :: IO [Package]
listGlobalPackages = parsePackages <$> readGhcPkg ["list"]

parsePackages :: String -> [Package]
parsePackages = map parsePackage . words

readGhcPkg :: [String] -> IO String
readGhcPkg args = readProcess "ghc-pkg" ("--no-user-package-db" : "--simple-output" : args) ""
