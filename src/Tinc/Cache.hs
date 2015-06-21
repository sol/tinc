{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
module Tinc.Cache (
  Sandbox
, cabalSandboxDirectory

, findPackageDb

, Cache(..)
, PackageLocation(..)
, readCache

#ifdef TEST
, readPackageGraph
#endif
) where

import           Prelude ()
import           Prelude.Compat

import qualified Data.Map as Map
import           Control.Monad.Compat
import           Data.List.Compat
import           Data.Maybe
import           System.Directory
import           System.FilePath

import           Package
import           Tinc.PackageGraph
import           Tinc.GhcInfo
import           Tinc.GhcPkg
import           Tinc.PackageDb
import           Tinc.Types
import           Util

data Sandbox

cabalSandboxDirectory :: FilePath
cabalSandboxDirectory = ".cabal-sandbox"

data Cache = Cache {
  _cacheGlobalPackages :: [Package]
, _cachePackageGraphs :: [PackageGraph PackageLocation]
}

data PackageLocation = GlobalPackage | PackageConfig (Path PackageConfig)
  deriving (Eq, Ord, Show)

readPackageGraph :: [(Package, a)] -> [Path PackageDb] -> IO (PackageGraph a)
readPackageGraph values packageDbs = do
  dot <- readGhcPkg packageDbs ["dot"]
  case fromDot values dot of
    Right graph -> return graph
    Left message -> die __FILE__ message

readCache :: GhcInfo -> Path CacheDir -> IO Cache
readCache ghcInfo cacheDir = do
  globalPackages <- listGlobalPackages
  let globalValues = map (, GlobalPackage) globalPackages
  sandboxes <- lookupSandboxes cacheDir
  cache <- forM sandboxes $ \ sandbox -> do
    packageDbPath <- findPackageDb sandbox
    packageDb <- readPackageDb packageDbPath
    let values = map (fmap PackageConfig) . Map.toList $ packageDbPackageConfigs packageDb
    readPackageGraph (globalValues ++ values) [ghcInfoGlobalPackageDb ghcInfo, packageDbPath]
  return (Cache globalPackages cache)

findPackageDb :: Path Sandbox -> IO (Path PackageDb)
findPackageDb sandbox = do
  xs <- getDirectoryContents sandboxDir
  case listToMaybe (filter isPackageDb xs) of
    Just p -> Path <$> canonicalizePath (sandboxDir </> p)
    Nothing -> die __FILE__ ("No package database found in " ++ show sandboxDir)
  where
    sandboxDir = path sandbox </> cabalSandboxDirectory

isPackageDb :: FilePath -> Bool
isPackageDb = ("-packages.conf.d" `isSuffixOf`)

lookupSandboxes :: Path CacheDir -> IO [Path Sandbox]
lookupSandboxes (Path cacheDir) = map Path <$> listDirectories cacheDir
