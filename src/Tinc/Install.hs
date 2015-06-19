{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinc.Install (
  Sandbox
, PackageConfig
, installDependencies
, cabalSandboxDirectory

-- exported for testing
, findReusablePackages
, findPackageDb
, extractPackages
, isPackageDb
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
    packageDb <- findPackageDb sandbox
    cacheGraph <- readPackageGraph [ghcInfoGlobalPackageDb ghcInfo, packageDb]
    let packages = nubBy ((==) `on` packageName) (installPlan ++ globalPackages)
        reusable = calculateReusablePackages packages cacheGraph
    lookupPackages packageDb reusable
  let reusablePackages = nubBy ((==) `on` fst) cachedPackages
      missingPackages = installPlan \\ map fst reusablePackages
  return (missingPackages, reusablePackages)

lookupSandboxes :: Path CacheDir -> IO [Path Sandbox]
lookupSandboxes (Path cacheDir) = map Path <$> listDirectories cacheDir

cloneSandbox :: Path Sandbox -> IO ()
cloneSandbox source = do
  sourcePackageDb <- findPackageDb source
  packages <- extractPackages sourcePackageDb
  initSandbox packages

findPackageDb :: Path Sandbox -> IO (Path PackageDb)
findPackageDb sandbox = do
  xs <- getDirectoryContents sandboxDir
  case listToMaybe (filter isPackageDb xs) of
    Just p -> Path <$> canonicalizePath (sandboxDir </> p)
    Nothing -> die ("package db not found in " ++ sandboxDir)
  where
    sandboxDir = path sandbox </> cabalSandboxDirectory

isPackageDb :: FilePath -> Bool
isPackageDb = ("-packages.conf.d" `isSuffixOf`)

extractPackages :: Path PackageDb -> IO [Path PackageConfig]
extractPackages packageDb = do
  packages <- listPackages [packageDb]
  map snd <$> lookupPackages packageDb packages

findPackageConfigs :: Path PackageDb -> IO [FilePath]
findPackageConfigs packageDb =
  filter (".conf" `isSuffixOf`) <$> getDirectoryContents (path packageDb)

lookupPackages :: Path PackageDb -> [Package] -> IO [(Package, Path PackageConfig)]
lookupPackages packageDb packages = do
  packageConfigs <- findPackageConfigs packageDb
  fmap catMaybes . forM packages $ \ package ->
    case lookupPackage package packageConfigs of
      Right (Just packageConfig) -> return $ Just (package, Path $ path packageDb </> packageConfig)
      Right Nothing -> return Nothing
      Left err -> die err

registerPackageConfigs :: Path PackageDb -> [Path PackageConfig] -> IO ()
registerPackageConfigs packageDb packages = do
  forM_ packages $ \ package ->
    copyFile (path package) (path packageDb </> takeFileName (path package))
  recache packageDb

recache :: Path PackageDb -> IO ()
recache packageDb = callProcess "ghc-pkg" ["--no-user-package-db", "recache", "--package-db", path packageDb]
