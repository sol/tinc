{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Tinc.Install (
  installDependencies
#ifdef TEST
, cabalInstallPlan
, generateCabalFile
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Catch
import           Control.Monad.Compat
import           Control.Monad.IO.Class
import           Data.Function
import           Data.List.Compat
import           System.Directory
import           System.IO.Temp

import           Tinc.Cache
import           Tinc.Fail
import           Tinc.GhcInfo
import           Tinc.Git
import           Tinc.Hpack
import           Tinc.Package
import           Tinc.PackageGraph
import           Tinc.Process
import           Tinc.Sandbox
import           Tinc.Types
import           Util

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
  _installPlanReusable :: [(Package, Path PackageConfig)]
, _installPlanMissing :: [Package]
} deriving (Eq, Show)

createInstallPlan :: GhcInfo -> Path CacheDir -> [Package] -> IO InstallPlan
createInstallPlan ghcInfo cacheDir installPlan = do
  cache <- readCache ghcInfo cacheDir
  let reusable = findReusablePackages cache installPlan
      missing = installPlan \\ map fst reusable
  return (InstallPlan reusable missing)

solveDependencies :: Path GitCache -> IO [Package]
solveDependencies gitCache = extractGitDependencies >>= mapM (clone gitCache) >>= cabalInstallPlan gitCache

cabalInstallPlan :: (MonadIO m, MonadMask m, Fail m, Process m) => Path GitCache -> [CachedGitDependency] -> m [Package]
cabalInstallPlan gitCache gitDependencies = withSystemTempDirectory "tinc" $ \dir -> do
  cabalFile <- liftIO generateCabalFile
  let command :: [String]
      command = "install" : "--only-dependencies" : "--enable-tests" : "--dry-run" : []
  withCurrentDirectory dir $ do
    liftIO $ uncurry writeFile cabalFile
    _ <- initSandbox (map (cachedGitDependencyPath gitCache) gitDependencies) []
    map addGitRevision <$> (readProcess "cabal" command "" >>= parseInstallPlan)
  where
    addGitRevision :: Package -> Package
    addGitRevision p@(Package name version) = case lookup name revisions of
      Just rev -> Package name version {versionGitRevision = Just rev}
      Nothing -> p

    revisions = [(name, rev) | CachedGitDependency name rev <- gitDependencies]

generateCabalFile :: IO (FilePath, String)
generateCabalFile = do
  files <- filter (".cabal" `isSuffixOf`) <$> getDirectoryContents "."
  case files of
    [file] -> (,) file <$> readFile file
    [] -> die "No cabal file found."
    _ -> die "Multiple cabal files found."

printInstallPlan :: InstallPlan -> IO ()
printInstallPlan (InstallPlan reusable missing) = do
  mapM_ (putStrLn . ("Reusing " ++) . showPackage) (map fst reusable)
  mapM_ (putStrLn . ("Installing " ++) . showPackage) missing

realizeInstallPlan :: Path CacheDir -> Path GitCache -> InstallPlan -> IO ()
realizeInstallPlan cacheDir gitCache (InstallPlan reusable missing) =
  packageConfigs >>= void . initSandbox []
  where
    packageConfigs
      | null missing = return (map snd reusable)
      | otherwise = populateCache cacheDir gitCache missing reusable

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
