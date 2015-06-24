{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Tinc.Install (
  installDependencies
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

import           Tinc.Package
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
      createInstallPlan ghcInfo cacheDir
  >>= tee printInstallPlan
  >>= unless dryRun . realizeInstallPlan cacheDir
  where
    tee :: Monad m => (a -> m ()) -> a -> m a
    tee action a = action a >> return a

data InstallPlan = InstallPlan {
  _installPlanAll :: [Package]
, _installPlanReusable :: [(Package, Path PackageConfig)]
, _installPlanMissing :: [Package]
} deriving (Eq, Show)

createInstallPlan :: GhcInfo -> Path CacheDir -> IO InstallPlan
createInstallPlan ghcInfo cacheDir = do
  installPlan <- cabalInstallPlan
  cache <- readCache ghcInfo cacheDir
  let reusable = findReusablePackages cache installPlan
      missing = installPlan \\ map fst reusable
  return (InstallPlan installPlan reusable missing)

cabalInstallPlan :: IO [Package]
cabalInstallPlan = parseInstallPlan <$> readProcess "cabal" command ""
  where
    command :: [String]
    command = words "--ignore-sandbox --no-require-sandbox install --only-dependencies --enable-tests --dry-run --package-db=clear --package-db=global"

printInstallPlan :: InstallPlan -> IO ()
printInstallPlan (InstallPlan _ reusable missing) = do
  mapM_ (putStrLn . ("Reusing " ++) . showPackage) (map fst reusable)
  mapM_ (putStrLn . ("Installing " ++) . showPackage) missing

realizeInstallPlan :: Path CacheDir -> InstallPlan -> IO ()
realizeInstallPlan cacheDir (InstallPlan installPlan (map snd -> reusable) missing) = packageConfigs >>= initSandbox
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

findReusablePackages :: Cache -> [Package] -> [(Package, Path PackageConfig)]
findReusablePackages (Cache globalPackages packageGraphs) installPlan = reusablePackages
  where
    reusablePackages :: [(Package, Path PackageConfig)]
    reusablePackages = nubBy ((==) `on` fst) (concatMap findReusable packageGraphs)

    findReusable :: PackageGraph PackageLocation -> [(Package, Path PackageConfig)]
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
