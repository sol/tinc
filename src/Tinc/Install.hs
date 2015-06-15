{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinc.Install (
  Sandbox
, PackageDB
, PackageConfig
, Cache
, installDependencies
, cabalSandboxDirectory

-- exported for testing
, findReusablePackages
, findPackageDB
, extractPackages
, isPackageDB
, realizeInstallPlan
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Exception
import           Control.Monad.Compat
import           Data.Function
import           Data.List.Compat
import           Data.Maybe
import           System.Directory
import           System.Exit.Compat
import           System.FilePath
import           System.IO.Temp
import           System.Process

import           Tinc.Types
import           Tinc.Setup
import           Tinc.GhcPkg
import           Tinc.GhcInfo
import           Package
import           PackageGraph
import           Util

data Sandbox
data PackageConfig

cabalSandboxDirectory :: FilePath
cabalSandboxDirectory = ".cabal-sandbox"

currentDirectory :: Path Sandbox
currentDirectory = "."

initSandbox :: [Path PackageConfig] -> IO ()
initSandbox packageConfigs = do
  deleteSandbox
  callCommand "cabal sandbox init"
  packageDB <- findPackageDB currentDirectory
  registerPackageConfigs packageDB packageConfigs

deleteSandbox :: IO ()
deleteSandbox = do
  exists <- doesDirectoryExist cabalSandboxDirectory
  when exists (callCommand "cabal sandbox delete")

installDependencies :: Bool -> Path Cache -> IO ()
installDependencies dryRun cache = do
  cabalInstallPlan >>= realizeInstallPlan dryRun cache

realizeInstallPlan :: Bool -> Path Cache -> [Package] -> IO ()
realizeInstallPlan dryRun cache installPlan = do
  (missing, reusable)  <- findReusablePackages cache installPlan
  printInstallPlan reusable missing
  unless dryRun (createProjectSandbox cache installPlan missing reusable)

cabalInstallPlan :: IO [Package]
cabalInstallPlan = parseInstallPlan <$> readProcess "cabal" command ""
  where
    command :: [String]
    command = words "--ignore-sandbox --no-require-sandbox install --only-dependencies --enable-tests --dry-run --package-db=clear --package-db=global"

printInstallPlan :: [Path PackageConfig] -> [Package] -> IO ()
printInstallPlan reusable missing = do
  mapM_ (putStrLn . ("reusing " ++) . path) reusable
  mapM_ (putStrLn . ("installing " ++) . showPackage) missing

createProjectSandbox :: Path Cache -> [Package] -> [Package] -> [Path PackageConfig] -> IO ()
createProjectSandbox cache installPlan missing reusable
  | null missing = initSandbox reusable
  | otherwise = createCacheSandbox cache installPlan reusable

createCacheSandbox :: Path Cache -> [Package] -> [Path PackageConfig] -> IO ()
createCacheSandbox cache installPlan reusable = do
  basename <- takeBaseName <$> getCurrentDirectory
  sandbox <- createTempDirectory (path cache) (basename ++ "-")
  create sandbox reusable `onException` removeDirectoryRecursive sandbox
  cloneSandbox (Path sandbox)
  where
    create sandbox cachedPackages = do
      withCurrentDirectory sandbox $ do
        initSandbox cachedPackages
        callProcess "cabal" ("install" : map showPackage installPlan)

findReusablePackages :: Path Cache -> [Package] -> IO ([Package], [Path PackageConfig])
findReusablePackages cache installPlan = do
  sandboxes <- lookupSandboxes cache
  globalPackages <- listGlobalPackages
  cachedPackages <- fmap concat . forM sandboxes $ \ sandbox -> do
    packageDB <- findPackageDB sandbox
    cacheGraph <- readPackageGraph packageDB
    let packages = nubBy ((==) `on` packageName) (installPlan ++ globalPackages)
        reusable = calculateReusablePackages packages cacheGraph
    lookupPackages packageDB reusable
  let (reusablePackages, reusablePackageConfigs) = unzip $ nubBy ((==) `on` fst) cachedPackages
      missingPackages = installPlan \\ reusablePackages
  return (missingPackages, reusablePackageConfigs)

lookupSandboxes :: Path Cache -> IO [Path Sandbox]
lookupSandboxes (Path cache) = map Path <$> listDirectories cache

cloneSandbox :: Path Sandbox -> IO ()
cloneSandbox source = do
  sourcePackageDB <- findPackageDB source
  packages <- extractPackages sourcePackageDB
  initSandbox packages

findPackageDB :: Path Sandbox -> IO (Path PackageDB)
findPackageDB sandbox = do
  xs <- getDirectoryContents sandboxDir
  case listToMaybe (filter isPackageDB xs) of
    Just p -> Path <$> canonicalizePath (sandboxDir </> p)
    Nothing -> die ("package db not found in " ++ sandboxDir)
  where
    sandboxDir = path sandbox </> cabalSandboxDirectory

isPackageDB :: FilePath -> Bool
isPackageDB = ("-packages.conf.d" `isSuffixOf`)

extractPackages :: Path PackageDB -> IO [Path PackageConfig]
extractPackages packageDB = do
  packages <- listPackages packageDB
  map snd <$> lookupPackages packageDB packages

readPackageGraph :: Path PackageDB -> IO PackageGraph
readPackageGraph (Path packageDB) = do
  Path globalPackageDB <- ghcInfoGlobalPackageDB <$> ghcInfo
  dot <- readGhcPkg ["--package-db", globalPackageDB, "--package-db", packageDB, "dot"]
  case fromDot dot of
    Right graph -> return graph
    Left message -> die message

findPackageConfigs :: Path PackageDB -> IO [FilePath]
findPackageConfigs packageDB =
  filter (".conf" `isSuffixOf`) <$> getDirectoryContents (path packageDB)

lookupPackages :: Path PackageDB -> [Package] -> IO [(Package, Path PackageConfig)]
lookupPackages packageDB packages = do
  packageConfigs <- findPackageConfigs packageDB
  fmap catMaybes . forM packages $ \ package ->
    case lookupPackage package packageConfigs of
      Right (Just packageConfig) -> return $ Just (package, Path $ path packageDB </> packageConfig)
      Right Nothing -> return Nothing
      Left err -> die err

registerPackageConfigs :: Path PackageDB -> [Path PackageConfig] -> IO ()
registerPackageConfigs packageDB packages = do
  forM_ packages $ \ package ->
    copyFile (path package) (path packageDB </> takeFileName (path package))
  recache packageDB

recache :: Path PackageDB -> IO ()
recache packageDB = callProcess "ghc-pkg" ["--no-user-package-db", "recache", "--package-db", path packageDB]
