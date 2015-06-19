module Tinc.PackageDb where

import           Prelude ()
import           Prelude.Compat

import           Control.Exception
import           Data.List.Compat
import           Data.Map (Map)
import qualified Data.Map as Map
import           System.Directory
import           System.FilePath

import           Package
import           Tinc.Types

data PackageConfig

data PackageDb = PackageDb {
  packageDbPath :: Path PackageDb
, packageDbPackageConfigs :: Map Package (Path PackageConfig)
} deriving (Eq, Show)

readPackageDb :: Path PackageDb -> IO PackageDb
readPackageDb p = do
  packageConfigs <- findPackageConfigs p
  let packages = map packageFromPackageConfig packageConfigs
  return $ PackageDb p (Map.fromList $ zip packages (map (Path . (path p </>)) packageConfigs))

lookupPackageConfig :: PackageDb -> Package -> IO (Path PackageConfig)
lookupPackageConfig (PackageDb (Path p) packageConfigs) package = case Map.lookup package packageConfigs of
  Just packageConfig -> return packageConfig
  Nothing -> throwIO (ErrorCall $ "no package config found for " ++ showPackage package ++ " in " ++ p)

findPackageConfigs :: Path PackageDb -> IO [FilePath]
findPackageConfigs packageDb =
  filter (".conf" `isSuffixOf`) <$> getDirectoryContents (path packageDb)

packageFromPackageConfig :: FilePath -> Package
packageFromPackageConfig packageConfig =
  case break (== '-') . drop 1 . dropWhile (/= '-') . reverse $ packageConfig of
    (version, name) -> Package (reverse $ drop 1 name) (reverse version)
