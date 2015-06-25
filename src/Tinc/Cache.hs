{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
module Tinc.Cache (
  Sandbox
, cabalSandboxDirectory

, findPackageDb

, PackageConfig
, listPackageConfigs

, Cache(..)
, PackageLocation(..)
, readCache

#ifdef TEST
, readPackageGraph
, packageFromPackageConfig
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Compat
import           Data.List.Compat
import           Data.Maybe
import           System.Directory
import           System.FilePath

import           Tinc.Package
import           Tinc.PackageGraph
import           Tinc.GhcInfo
import           Tinc.GhcPkg
import           Tinc.Types
import           Tinc.Fail
import           Util

data Sandbox

cabalSandboxDirectory :: FilePath
cabalSandboxDirectory = ".cabal-sandbox"

data PackageConfig

listPackageConfigs :: Path PackageDb -> IO [(Package, Path PackageConfig)]
listPackageConfigs p = do
  packageConfigs <- filter (".conf" `isSuffixOf`) <$> getDirectoryContents (path p)
  let packages = map packageFromPackageConfig packageConfigs
      absolutePackageConfigs = map (Path . (path p </>)) packageConfigs
  return (zip packages absolutePackageConfigs)

packageFromPackageConfig :: FilePath -> Package
packageFromPackageConfig = parsePackage . reverse . drop 1 . dropWhile (/= '-') . reverse

data Cache = Cache {
  _cacheGlobalPackages :: [Package]
, _cachePackageGraphs :: [PackageGraph PackageLocation]
}

data PackageLocation = GlobalPackage | PackageConfig (Path PackageConfig)
  deriving (Eq, Ord, Show)

readPackageGraph :: (Fail m, GhcPkg m) => [(Package, a)] -> [Path PackageDb] -> m (PackageGraph a)
readPackageGraph values packageDbs = readGhcPkg packageDbs ["dot"] >>= fromDot values

readCache :: GhcInfo -> Path CacheDir -> IO Cache
readCache ghcInfo cacheDir = do
  globalPackages <- listGlobalPackages
  let globalValues = map (, GlobalPackage) globalPackages
  sandboxes <- lookupSandboxes cacheDir
  cache <- forM sandboxes $ \ sandbox -> do
    packageDbPath <- findPackageDb sandbox
    packageConfigs <- listPackageConfigs packageDbPath
    let values = map (fmap PackageConfig) packageConfigs
    readPackageGraph (globalValues ++ values) [ghcInfoGlobalPackageDb ghcInfo, packageDbPath]
  return (Cache globalPackages cache)

findPackageDb :: Path Sandbox -> IO (Path PackageDb)
findPackageDb sandbox = do
  xs <- getDirectoryContents sandboxDir
  case listToMaybe (filter isPackageDb xs) of
    Just p -> Path <$> canonicalizePath (sandboxDir </> p)
    Nothing -> dieLoc __FILE__ ("No package database found in " ++ show sandboxDir)
  where
    sandboxDir = path sandbox </> cabalSandboxDirectory

isPackageDb :: FilePath -> Bool
isPackageDb = ("-packages.conf.d" `isSuffixOf`)

lookupSandboxes :: Path CacheDir -> IO [Path Sandbox]
lookupSandboxes (Path cacheDir) = map Path <$> listDirectories cacheDir
