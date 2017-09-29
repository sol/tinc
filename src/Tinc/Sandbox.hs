{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tinc.Sandbox (
  PackageConfig
, Sandbox

, findPackageDb
, touchPackageCache
, initSandbox

, cabalSandboxDirectory
, cabalSandboxBinDirectory

, cachedListPackages

#ifdef TEST
, listPackages
, packageFromPackageConfig
, registerPackage
#endif
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import           Data.Maybe
import           System.Directory hiding (getDirectoryContents)
import           System.FilePath
import           System.PosixCompat.Files

import           Util
import           Tinc.Fail
import           Tinc.GhcPkg
import           Tinc.Package
import           Tinc.Process
import           Tinc.Types
import           Tinc.AddSource

data PackageConfig

data Sandbox

currentDirectory :: Path Sandbox
currentDirectory = "."

touchPackageCache :: Path PackageDb -> IO ()
touchPackageCache packageDb = touchFile (path packageDb </> "package.cache")

initSandbox :: (MonadIO m, Fail m, MonadProcess m) => [Path AddSource] -> [Path PackageConfig] -> m (Path PackageDb)
initSandbox addSourceDependencies packageConfigs = do
  deleteSandbox
  callProcessM "cabal" ["sandbox", "init"]
  packageDb <- findPackageDb currentDirectory
  registerPackageConfigs packageDb packageConfigs
  mapM_ (\ dep -> callProcessM "cabal" ["sandbox", "add-source", path dep]) addSourceDependencies
  liftIO $ createDirectoryIfMissing False cabalSandboxBinDirectory
  return packageDb

deleteSandbox :: (MonadIO m, MonadProcess m) => m ()
deleteSandbox = do
  exists <- liftIO $ doesDirectoryExist cabalSandboxDirectory
  when exists (callProcessM "cabal" ["sandbox", "delete"])

findPackageDb :: (MonadIO m, Fail m) => Path Sandbox -> m (Path PackageDb)
findPackageDb sandbox = do
  xs <- liftIO $ getDirectoryContents sandboxDir
  case listToMaybe (filter isPackageDb xs) of
    Just p -> liftIO $ Path <$> canonicalizePath (sandboxDir </> p)
    Nothing -> dieLoc ("No package database found in " ++ show sandboxDir)
  where
    sandboxDir = path sandbox </> cabalSandboxDirectory

isPackageDb :: FilePath -> Bool
isPackageDb = ("-packages.conf.d" `isSuffixOf`)

cabalSandboxDirectory :: FilePath
cabalSandboxDirectory = ".cabal-sandbox"

cabalSandboxBinDirectory :: FilePath
cabalSandboxBinDirectory = cabalSandboxDirectory </> "bin"

registerPackageConfigs :: (MonadIO m, MonadProcess m) => Path PackageDb -> [Path PackageConfig] -> m ()
registerPackageConfigs _packageDb [] = return ()
registerPackageConfigs packageDb packages = do
  liftIO $ forM_ packages (registerPackage packageDb)
  recache packageDb

registerPackage :: Path PackageDb -> Path PackageConfig -> IO ()
registerPackage packageDb package = linkFile (path package) (path packageDb)

cachedListPackages :: MonadIO m => Path PackageDb -> m [(SimplePackage, Path PackageConfig)]
cachedListPackages p = do
  map (fmap Path) <$> cachedIOAfterStore (liftIO $ touchPackageCache p) cacheFile (listPackages p)
  where
    cacheFile = path p </> "packages.v2"

listPackages :: MonadIO m => Path PackageDb -> m [(SimplePackage, FilePath)]
listPackages p = do
  packageConfigs <- liftIO $ filter (".conf" `isSuffixOf`) <$> getDirectoryContents (path p)
  absolutePackageConfigs <- liftIO . mapM canonicalizePath $ map (path p </>) packageConfigs
  packages <- mapM (liftIO . packageFromPackageConfig) absolutePackageConfigs
  return (zip packages absolutePackageConfigs)

packageFromPackageConfig :: FilePath -> IO SimplePackage
packageFromPackageConfig conf = do
  input <- readFile conf
  case parsePackageConfig (lines input) of
    Just x -> return x
    Nothing -> dieLoc (conf ++ ": parse error")

parsePackageConfig :: [String] -> Maybe SimplePackage
parsePackageConfig input = SimplePackage <$> name <*> version
  where
    name = readField "name" input
    version = readField "version" input
    readField field = listToMaybe . mapMaybe (stripPrefix $ field ++ ": ")

recache :: MonadProcess m => Path PackageDb -> m ()
recache packageDb = callProcessM "ghc-pkg" ["--no-user-package-conf", "recache", "--package-conf", path packageDb]
