{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinc.Install (
  Sandbox
, PackageConfig
, installDependencies

#ifdef TEST
, findReusablePackages
, realizeInstallPlan
#endif
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
import           Tinc.Types
import           Tinc.GhcPkg
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
  let (missing, reusable) = findReusablePackages cache installPlan
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
createProjectSandbox cacheDir installPlan missing reusable = packageConfigs >>= initSandbox
  where
    packageConfigs
      | null missing = return reusable
      | otherwise = populateCache cacheDir installPlan reusable

populateCache :: Path CacheDir -> [Package] -> [Path PackageConfig] -> IO [Path PackageConfig]
populateCache cacheDir installPlan reusable = do
  basename <- takeBaseName <$> getCurrentDirectory
  sandbox <- createTempDirectory (path cacheDir) (basename ++ "-")
  populate sandbox reusable `onException` removeDirectoryRecursive sandbox
  list sandbox
  where
    populate sandbox cachedPackages = do
      withCurrentDirectory sandbox $ do
        initSandbox cachedPackages
        callProcess "cabal" ("install" : map showPackage installPlan)

    list :: FilePath -> IO [Path PackageConfig]
    list sandbox = do
      sourcePackageDb <- findPackageDb (Path sandbox)
      map snd <$> listPackageConfigs sourcePackageDb

findReusablePackages :: Cache -> [Package] -> ([Package], [(Package, Path PackageConfig)])
findReusablePackages (Cache globalPackages packageGraphs) installPlan = (missingPackages, reusablePackages)
  where
    reusablePackages = nubBy ((==) `on` fst) (concatMap findReusable packageGraphs)
    missingPackages = installPlan \\ map fst reusablePackages

    findReusable cacheGraph =
      [(p, c) | (p, PackageConfig c)  <- calculateReusablePackages packages cacheGraph]
      where
        packages = nubBy ((==) `on` packageName) (installPlan ++ globalPackages)

registerPackageConfigs :: Path PackageDb -> [Path PackageConfig] -> IO ()
registerPackageConfigs packageDb packages = do
  forM_ packages $ \ package ->
    copyFile (path package) (path packageDb </> takeFileName (path package))
  recache packageDb

recache :: Path PackageDb -> IO ()
recache packageDb = callProcess "ghc-pkg" ["--no-user-package-db", "recache", "--package-db", path packageDb]
