module Tinc.GhcPkg (
  readGhcPkg
, listGlobalPackages
) where

import           Prelude ()
import           Prelude.Compat

import           System.Process

import           Package

listGlobalPackages :: IO [Package]
listGlobalPackages = map parsePackage . words <$> readGhcPkg ["list"]

readGhcPkg :: [String] -> IO String
readGhcPkg args = readProcess "ghc-pkg" ("--no-user-package-db" : "--simple-output" : args) ""
