{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tinc.Sandbox (
  PackageConfig
, Sandbox

, findPackageDb
, initSandbox
, recache

, cabalSandboxDirectory
, cabalSandboxBinDirectory

, listPackages

#ifdef TEST
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

listPackages :: MonadIO m => Path PackageDb -> m [(Package, Path PackageConfig)]
listPackages p = do
  packageConfigs <- liftIO $ filter (".conf" `isSuffixOf`) <$> getDirectoryContents (path p)
  absolutePackageConfigs <- liftIO . mapM canonicalizePath $ map (path p </>) packageConfigs
  let packages = map packageFromPackageConfig packageConfigs
  return (zip packages (map Path absolutePackageConfigs))

packageFromPackageConfig :: FilePath -> Package
packageFromPackageConfig = parsePackage . reverse . drop 1 . dropWhile (/= '-') . reverse

recache :: MonadProcess m => Path PackageDb -> m ()
recache packageDb = callProcessM "ghc-pkg" ["--no-user-package-db", "recache", "--package-db", path packageDb]
