{-# LANGUAGE CPP #-}
module Tinc.PackageDb (
  PackageDb(..)
, PackageConfig
, readPackageDb
, lookupPackageConfig
, allPackageConfigs
#ifdef TEST
, packageFromPackageConfig
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Data.List.Compat
import           Data.Map (Map)
import qualified Data.Map as Map
import           System.Directory
import           System.FilePath

import           Package
import           Tinc.Types
import           Util

data PackageConfig

data PackageDb = PackageDb {
  _packageDbPath :: Path PackageDb
, packageDbPackageConfigs :: Map Package (Path PackageConfig)
} deriving (Eq, Show)

readPackageDb :: Path PackageDb -> IO PackageDb
readPackageDb p = do
  packageConfigs <- filter (".conf" `isSuffixOf`) <$> getDirectoryContents (path p)
  let packages = map packageFromPackageConfig packageConfigs
      absolutePackageConfigs = map (Path . (path p </>)) packageConfigs
  return $ PackageDb p (Map.fromList $ zip packages absolutePackageConfigs)

lookupPackageConfig :: PackageDb -> Package -> IO (Path PackageConfig)
lookupPackageConfig (PackageDb (Path p) packageConfigs) package = case Map.lookup package packageConfigs of
  Just packageConfig -> return packageConfig
  Nothing -> die __FILE__ ("No package config found for " ++ showPackage package ++ " in " ++ p)

allPackageConfigs :: PackageDb -> [Path PackageConfig]
allPackageConfigs = Map.elems . packageDbPackageConfigs

packageFromPackageConfig :: FilePath -> Package
packageFromPackageConfig packageConfig =
  case break (== '-') . drop 1 . dropWhile (/= '-') . reverse $ packageConfig of
    (version, name) -> Package (reverse $ drop 1 name) (reverse version)
