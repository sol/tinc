{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinc.Install (
  Sandbox
, PackageConfig
, installDependencies
, cabalSandboxDirectory

-- exported for testing
, findReusablePackages
, findPackageDB
, extractPackageConfigs
, isPackageDB
, realizeInstallPlan
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Exception
import           Control.Monad.Compat
import           Data.Function
import           Data.List.Compat
import qualified Data.Map as Map
import           Data.Maybe
import           System.Directory
import           System.Exit.Compat
import           System.FilePath
import           System.IO.Temp
import           System.Process

import           Package
import           PackageGraph
import           Tinc.GhcInfo
import           Tinc.GhcPkg
import           Tinc.PackageDb
import           Tinc.Types
import           Util

data Sandbox

cabalSandboxDirectory :: FilePath
cabalSandboxDirectory = ".cabal-sandbox"

currentDirectory :: Path Sandbox
currentDirectory = "."

initSandbox :: [Path PackageConfig] -> IO ()
initSandbox packageConfigs = do
  deleteSandbox
  callCommand "cabal sandbox init"
  packageDb <- findPackageDB currentDirectory
  registerPackageConfigs packageDb packageConfigs

deleteSandbox :: IO ()
deleteSandbox = do
  exists <- doesDirectoryExist cabalSandboxDirectory
  when exists (callCommand "cabal sandbox delete")

installDependencies :: GhcInfo -> Bool -> Path CacheDir -> IO ()
installDependencies ghcInfo dryRun cacheDir = do
  cabalInstallPlan >>= realizeInstallPlan ghcInfo dryRun cacheDir

realizeInstallPlan :: GhcInfo -> Bool -> Path CacheDir -> [Package] -> IO ()
realizeInstallPlan ghcInfo dryRun cacheDir installPlan = do
  (missing, reusable) <- findReusablePackages ghcInfo cacheDir installPlan
  printInstallPlan reusable missing
  unless dryRun (createProjectSandbox cacheDir installPlan missing (map snd reusable))

cabalInstallPlan :: IO [Package]
cabalInstallPlan = parseInstallPlan <$> readProcess "cabal" command ""
  where
    command :: [String]
    command = words "--ignore-sandbox --no-require-sandbox install --only-dependencies --enable-tests --dry-run --package-db=clear --package-db=global"

printInstallPlan :: [(Package, Path PackageConfig)] -> [Package] -> IO ()
printInstallPlan reusable missing = do
  mapM_ (putStrLn . ("Reusing " ++) . showPackage) (map fst reusable)
  mapM_ (putStrLn . ("Installing " ++) . showPackage) missing

createProjectSandbox :: Path CacheDir -> [Package] -> [Package] -> [Path PackageConfig] -> IO ()
createProjectSandbox cacheDir installPlan missing reusable
  | null missing = initSandbox reusable
  | otherwise = createCacheSandbox cacheDir installPlan reusable

createCacheSandbox :: Path CacheDir -> [Package] -> [Path PackageConfig] -> IO ()
createCacheSandbox cacheDir installPlan reusable = do
  basename <- takeBaseName <$> getCurrentDirectory
  sandbox <- createTempDirectory (path cacheDir) (basename ++ "-")
  create sandbox reusable `onException` removeDirectoryRecursive sandbox
  cloneSandbox (Path sandbox)
  where
    create sandbox cachedPackages = do
      withCurrentDirectory sandbox $ do
        initSandbox cachedPackages
        callProcess "cabal" ("install" : map showPackage installPlan)

findReusablePackages :: GhcInfo -> Path CacheDir -> [Package] -> IO ([Package], [(Package, Path PackageConfig)])
findReusablePackages ghcInfo cacheDir installPlan = do
  sandboxes <- lookupSandboxes cacheDir
  globalPackages <- listGlobalPackages
  cachedPackages <- fmap concat . forM sandboxes $ \ sandbox -> do
    packageDbPath <- findPackageDB sandbox
    cacheGraph <- readPackageGraph [ghcInfoGlobalPackageDB ghcInfo, packageDbPath]
    let packages = nubBy ((==) `on` packageName) (installPlan ++ globalPackages)
        reusable = calculateReusablePackages packages cacheGraph \\ globalPackages
    packageDb <- readPackageDb packageDbPath
    zip reusable <$> mapM (lookupPackageConfig packageDb) reusable
  let reusablePackages = nubBy ((==) `on` fst) cachedPackages
      missingPackages = installPlan \\ map fst reusablePackages
  return (missingPackages, reusablePackages)

lookupSandboxes :: Path CacheDir -> IO [Path Sandbox]
lookupSandboxes (Path cacheDir) = map Path <$> listDirectories cacheDir

cloneSandbox :: Path Sandbox -> IO ()
cloneSandbox source = do
  sourcePackageDB <- findPackageDB source
  packages <- extractPackageConfigs sourcePackageDB
  initSandbox packages

findPackageDB :: Path Sandbox -> IO (Path PackageDb)
findPackageDB sandbox = do
  xs <- getDirectoryContents sandboxDir
  case listToMaybe (filter isPackageDB xs) of
    Just p -> Path <$> canonicalizePath (sandboxDir </> p)
    Nothing -> die ("package db not found in " ++ sandboxDir)
  where
    sandboxDir = path sandbox </> cabalSandboxDirectory

isPackageDB :: FilePath -> Bool
isPackageDB = ("-packages.conf.d" `isSuffixOf`)

extractPackageConfigs :: Path PackageDb -> IO [Path PackageConfig]
extractPackageConfigs packageDb = do
  Map.elems . packageDbPackageConfigs <$> readPackageDb packageDb

registerPackageConfigs :: Path PackageDb -> [Path PackageConfig] -> IO ()
registerPackageConfigs packageDb packages = do
  forM_ packages $ \ package ->
    copyFile (path package) (path packageDb </> takeFileName (path package))
  recache packageDb

recache :: Path PackageDb -> IO ()
recache packageDb = callProcess "ghc-pkg" ["--no-user-package-db", "recache", "--package-db", path packageDb]
