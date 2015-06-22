{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinc.Install (
  Sandbox
, PackageConfig
, installDependencies

-- exported for testing
, findReusablePackages
, realizeInstallPlan
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Exception
import           Control.Monad.Compat
import           Data.Function
import           Data.List.Compat
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           System.Process

import           Package
import           Tinc.PackageGraph
import           Tinc.Cache
import           Tinc.GhcInfo
import           Tinc.PackageDb
import           Tinc.Types
import           Util

currentDirectory :: Path Sandbox
currentDirectory = "."

initSandbox :: [Path PackageConfig] -> IO ()
initSandbox packageConfigs = do
  deleteSandbox
  callCommand "cabal sandbox init"
  packageDb <- findPackageDb currentDirectory
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
  cache <- readCache ghcInfo cacheDir
  (missing, reusable) <- findReusablePackages cache installPlan
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

findReusablePackages :: Cache -> [Package] -> IO ([Package], [(Package, Path PackageConfig)])
findReusablePackages (Cache globalPackages packageGraphs) installPlan = do
  cachedPackages <- fmap concat . forM packageGraphs $ \ (packageDbPath, cacheGraph) -> do
    let packages = nubBy ((==) `on` packageName) (installPlan ++ globalPackages)
        reusable = calculateReusablePackages packages cacheGraph \\ globalPackages
    packageDb <- readPackageDb packageDbPath
    zip reusable <$> mapM (lookupPackageConfig packageDb) reusable
  let reusablePackages = nubBy ((==) `on` fst) cachedPackages
      missingPackages = installPlan \\ map fst reusablePackages
  return (missingPackages, reusablePackages)

cloneSandbox :: Path Sandbox -> IO ()
cloneSandbox source = do
  sourcePackageDb <- findPackageDb source
  packages <- extractPackageConfigs sourcePackageDb
  initSandbox packages

extractPackageConfigs :: Path PackageDb -> IO [Path PackageConfig]
extractPackageConfigs packageDb = do
  allPackageConfigs <$> readPackageDb packageDb

registerPackageConfigs :: Path PackageDb -> [Path PackageConfig] -> IO ()
registerPackageConfigs packageDb packages = do
  forM_ packages $ \ package ->
    copyFile (path package) (path packageDb </> takeFileName (path package))
  recache packageDb

recache :: Path PackageDb -> IO ()
recache packageDb = callProcess "ghc-pkg" ["--no-user-package-db", "recache", "--package-db", path packageDb]
