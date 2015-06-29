{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Tinc.Install (
  installDependencies
#ifdef TEST
, cabalInstallPlan
, populateCache
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Catch
import           Control.Monad.Compat
import           Control.Monad.IO.Class
import           Data.Function
import           Data.List.Compat
import           Data.Yaml
import           System.Directory
import           System.FilePath
import           System.IO.Temp

import           Tinc.Cache
import           Tinc.Fail
import           Tinc.GhcInfo
import           Tinc.GhcPkg
import           Tinc.Git
import           Tinc.Hpack
import           Tinc.Package
import           Tinc.PackageGraph
import           Tinc.Process
import           Tinc.Types
import           Util

currentDirectory :: Path Sandbox
currentDirectory = "."

initSandbox :: (MonadIO m, Fail m, Process m) => [Path CachedGitDependency] -> [Path PackageConfig] -> m (Path PackageDb)
initSandbox gitDependencies packageConfigs = do
  deleteSandbox
  callProcess "cabal" ["sandbox", "init"]
  packageDb <- findPackageDb currentDirectory
  registerPackageConfigs packageDb packageConfigs
  mapM_ (\ dep -> callProcess "cabal" ["sandbox", "add-source", path dep]) gitDependencies
  return packageDb

deleteSandbox :: (MonadIO m, Process m) => m ()
deleteSandbox = do
  exists <- liftIO $ doesDirectoryExist cabalSandboxDirectory
  when exists (callProcess "cabal" ["sandbox", "delete"])

installDependencies :: GhcInfo -> Bool -> Path CacheDir -> Path GitCache -> IO ()
installDependencies ghcInfo dryRun cacheDir gitCache = do
      solveDependencies gitCache
  >>= createInstallPlan ghcInfo cacheDir
  >>= tee printInstallPlan
  >>= unless dryRun . realizeInstallPlan cacheDir gitCache
  where
    tee :: Monad m => (a -> m ()) -> a -> m a
    tee action a = action a >> return a

data InstallPlan = InstallPlan {
  _installPlanAll :: [Package]
, _installPlanReusable :: [(Package, Path PackageConfig)]
, _installPlanMissing :: [Package]
} deriving (Eq, Show)

createInstallPlan :: GhcInfo -> Path CacheDir -> [Package] -> IO InstallPlan
createInstallPlan ghcInfo cacheDir installPlan = do
  cache <- readCache ghcInfo cacheDir
  let reusable = findReusablePackages cache installPlan
      missing = installPlan \\ map fst reusable
  return (InstallPlan installPlan reusable missing)

solveDependencies :: Path GitCache -> IO [Package]
solveDependencies gitCache = extractGitDependencies >>= mapM (clone gitCache) >>= cabalInstallPlan gitCache

cabalInstallPlan :: (MonadIO m, MonadMask m, Fail m, Process m) => Path GitCache -> [CachedGitDependency] -> m [Package]
cabalInstallPlan gitCache gitDependencies = withSystemTempDirectory "tinc" $ \dir -> do
  target <- liftIO getCurrentDirectory
  let command :: [String]
      command = "install" : "--only-dependencies" : "--enable-tests" : "--dry-run" : [target]
  withCurrentDirectory dir $ do
    _ <- initSandbox (map (cachedGitDependencyPath gitCache) gitDependencies) []
    map addGitRevision <$> (readProcess "cabal" command "" >>= parseInstallPlan)
  where
    addGitRevision :: Package -> Package
    addGitRevision p@(Package name version) = case lookup name revisions of
      Just rev -> Package name version {versionGitRevision = Just rev}
      Nothing -> p

    revisions = [(name, rev) | CachedGitDependency name rev <- gitDependencies]

printInstallPlan :: InstallPlan -> IO ()
printInstallPlan (InstallPlan _ reusable missing) = do
  mapM_ (putStrLn . ("Reusing " ++) . showPackage) (map fst reusable)
  mapM_ (putStrLn . ("Installing " ++) . showPackage) missing

realizeInstallPlan :: Path CacheDir -> Path GitCache -> InstallPlan -> IO ()
realizeInstallPlan cacheDir gitCache (InstallPlan installPlan (map snd -> reusable) missing) =
  packageConfigs >>= void . initSandbox []
  where
    packageConfigs
      | null missing = return reusable
      | otherwise = populateCache cacheDir gitCache installPlan reusable

populateCache :: forall m . (MonadIO m, MonadMask m, Fail m, Process m) =>
  Path CacheDir -> Path GitCache -> [Package] -> [Path PackageConfig] -> m [Path PackageConfig]
populateCache cacheDir gitCache installPlan reusable = do
  basename <- takeBaseName <$> liftIO getCurrentDirectory
  sandbox <- liftIO $ createTempDirectory (path cacheDir) (basename ++ "-")
  populate sandbox reusable `onException` liftIO (removeDirectoryRecursive sandbox)
  list sandbox
  where
    populate sandbox cachedPackages = do
      withCurrentDirectory sandbox $ do
        packageDb <- initSandbox (map gitRevisionToPath gitRevisions) cachedPackages
        writeGitRevisions packageDb
        callProcess "cabal" ("install" : map showPackage installPlan)

    gitRevisionToPath :: GitRevision -> Path CachedGitDependency
    gitRevisionToPath GitRevision{..} = revisionToPath gitCache gitRevisionName gitRevisionRevision

    list :: FilePath -> m [Path PackageConfig]
    list sandbox = do
      sourcePackageDb <- findPackageDb (Path sandbox)
      map snd <$> listPackageConfigs sourcePackageDb

    writeGitRevisions packageDb
      | null gitRevisions = return ()
      | otherwise = do
          liftIO $ encodeFile (path packageDb </> "git-revisions.yaml") gitRevisions
          recache packageDb

    gitRevisions = [GitRevision name revision | Package name (Version _ (Just revision)) <- installPlan]

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

registerPackageConfigs :: (MonadIO m, Process m) => Path PackageDb -> [Path PackageConfig] -> m ()
registerPackageConfigs _packageDb [] = return ()
registerPackageConfigs packageDb packages = do
  forM_ packages $ \ package ->
    liftIO $ copyFile (path package) (path packageDb </> takeFileName (path package))
  recache packageDb

recache :: Process m => Path PackageDb -> m ()
recache packageDb = callProcess "ghc-pkg" ["--no-user-package-db", "recache", "--package-db", path packageDb]
