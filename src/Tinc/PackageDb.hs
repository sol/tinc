{-# LANGUAGE CPP #-}
module Tinc.PackageDb (
  PackageDb(..)
, PackageConfig
, readPackageDb
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

data PackageConfig

data PackageDb = PackageDb {
  packageDbPackageConfigs :: Map Package (Path PackageConfig)
} deriving (Eq, Show)

readPackageDb :: Path PackageDb -> IO PackageDb
readPackageDb p = do
  packageConfigs <- filter (".conf" `isSuffixOf`) <$> getDirectoryContents (path p)
  let packages = map packageFromPackageConfig packageConfigs
      absolutePackageConfigs = map (Path . (path p </>)) packageConfigs
  return $ PackageDb (Map.fromList $ zip packages absolutePackageConfigs)

allPackageConfigs :: PackageDb -> [Path PackageConfig]
allPackageConfigs = Map.elems . packageDbPackageConfigs

packageFromPackageConfig :: FilePath -> Package
packageFromPackageConfig packageConfig =
  case break (== '-') . drop 1 . dropWhile (/= '-') . reverse $ packageConfig of
    (version, name) -> Package (reverse $ drop 1 name) (reverse version)
