{-# LANGUAGE CPP #-}
module Tinc.Cache (
  Sandbox
, cabalSandboxDirectory

, findPackageDb

, Cache(..)
, readCache

#ifdef TEST
, readPackageGraph
#endif
) where

import           Prelude ()
import           Prelude.Compat

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
, _cachePackageGraphs :: [(Path PackageDb, PackageGraph)]
}

readPackageGraph :: [Path PackageDb] -> IO PackageGraph
readPackageGraph packageDbs = do
  dot <- readGhcPkg packageDbs ["dot"]
  packages <- listPackages packageDbs
  case fromDot packages dot of
    Right graph -> return graph
    Left message -> die __FILE__ message

readCache :: GhcInfo -> Path CacheDir -> IO Cache
readCache ghcInfo cacheDir = do
  sandboxes <- lookupSandboxes cacheDir
  cache <- forM sandboxes $ \ sandbox -> do
    packageDbPath <- findPackageDb sandbox
    (,) packageDbPath <$> readPackageGraph [ghcInfoGlobalPackageDb ghcInfo, packageDbPath]
  globalPackages <- listGlobalPackages
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
