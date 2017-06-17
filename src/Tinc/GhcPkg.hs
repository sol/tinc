{-# LANGUAGE ViewPatterns #-}
module Tinc.GhcPkg (
  PackageDb
, GhcPkg(..)
, listGlobalPackages
) where

import           System.Process

import           Tinc.Package
import           Tinc.Types

data PackageDb

class (Functor m, Applicative m, Monad m) => GhcPkg m where
  readGhcPkg :: [Path PackageDb] -> [String] -> m String

instance GhcPkg IO where
  readGhcPkg (packageDbsToArgs -> packageDbs) args = do
    readProcess "ghc-pkg" ("--no-user-package-conf" : "--simple-output" : packageDbs ++ args) ""

listGlobalPackages :: GhcPkg m => m [Package]
listGlobalPackages = parsePackages <$> readGhcPkg [] ["list"]
  where
    parsePackages :: String -> [Package]
    parsePackages = map parsePackage . words

packageDbsToArgs :: [Path PackageDb] -> [String]
packageDbsToArgs packageDbs = concatMap (\ packageDb -> ["--package-conf", path packageDb]) packageDbs
