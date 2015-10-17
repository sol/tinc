{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Tinc.Install (
  installDependencies
#ifdef TEST
, cabalInstallPlan
, copyFreezeFile
, generateCabalFile
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Catch
import           Control.Monad.Compat
import           Control.Monad.IO.Class
import           Data.List.Compat
import           System.Directory
import           System.IO.Temp
import           System.FilePath

import qualified Hpack.Config as Hpack
import           Tinc.Cache
import           Tinc.Config
import           Tinc.Fail
import           Tinc.GhcInfo
import qualified Tinc.Hpack as Hpack
import           Tinc.Package
import           Tinc.Process
import           Tinc.Sandbox
import           Tinc.Types
import           Util

installDependencies :: GhcInfo -> Bool -> Path CacheDir -> Path AddSourceCache -> IO ()
installDependencies ghcInfo dryRun cacheDir addSourceCache = do
      solveDependencies addSourceCache
  >>= createInstallPlan ghcInfo cacheDir
  >>= tee printInstallPlan
  >>= unless dryRun . realizeInstallPlan cacheDir addSourceCache
  where
    tee :: Monad m => (a -> m ()) -> a -> m a
    tee action a = action a >> return a

data InstallPlan = InstallPlan {
  _installPlanReusable :: [CachedPackage]
, _installPlanMissing :: [Package]
} deriving (Eq, Show)

createInstallPlan :: GhcInfo -> Path CacheDir -> [Package] -> IO InstallPlan
createInstallPlan ghcInfo cacheDir installPlan = do
  cache <- readCache ghcInfo cacheDir
  let reusable = findReusablePackages cache installPlan
      missing = installPlan \\ map cachedPackageName reusable
  return (InstallPlan reusable missing)

solveDependencies :: Path AddSourceCache -> IO [Package]
solveDependencies addSourceCache = do
  additionalDeps <- getAdditionalDependencies
  addSourceDependencies <- Hpack.extractsAddSourceDependencies addSourceCache additionalDeps
  cabalInstallPlan additionalDeps addSourceCache addSourceDependencies

cabalInstallPlan :: (MonadIO m, MonadMask m, Fail m, Process m) => [Hpack.Dependency] -> Path AddSourceCache -> [AddSource] -> m [Package]
cabalInstallPlan additionalDeps addSourceCache addSourceDependencies = withSystemTempDirectory "tinc" $ \dir -> do
  liftIO $ copyFreezeFile dir
  cabalFile <- liftIO (generateCabalFile additionalDeps)
  let command :: [String]
      command = "install" : "--only-dependencies" : "--enable-tests" : "--dry-run" : []
  withCurrentDirectory dir $ do
    liftIO $ uncurry writeFile cabalFile
    _ <- initSandbox (map (addSourcePath addSourceCache) addSourceDependencies) []
    map addAddSourceHash <$> (readProcess "cabal" command "" >>= parseInstallPlan)
  where
    addAddSourceHash :: Package -> Package
    addAddSourceHash p@(Package name version) = case lookup name addSourceHashes of
      Just rev -> Package name version {versionAddSourceHash = Just rev}
      Nothing -> p

    addSourceHashes = [(name, rev) | AddSource name rev <- addSourceDependencies]

copyFreezeFile :: FilePath -> IO ()
copyFreezeFile dst = do
  exists <- doesFileExist freezeFile
  when exists $ do
    copyFile freezeFile (dst </> freezeFile)
  where
    freezeFile = "cabal.config"

generateCabalFile :: [Hpack.Dependency] -> IO (FilePath, String)
generateCabalFile deps = do
  exists <- Hpack.doesConfigExist
  if exists
    then Hpack.render <$> Hpack.readConfig deps
    else do
      files <- filter (".cabal" `isSuffixOf`) <$> getDirectoryContents "."
      if (null deps || (not . null) files)
        then copyCabalFile files
        else do
          let package = Hpack.mkPackage deps
          return (Hpack.render package)
  where
    copyCabalFile files = do
      case files of
        [file] -> (,) file <$> readFile file
        [] -> die "No cabal file found."
        _ -> die "Multiple cabal files found."

printInstallPlan :: InstallPlan -> IO ()
printInstallPlan (InstallPlan reusable missing) = do
  mapM_ (putStrLn . ("Reusing " ++) . showPackage) (map cachedPackageName reusable)
  mapM_ (putStrLn . ("Installing " ++) . showPackage) missing

realizeInstallPlan :: Path CacheDir -> Path AddSourceCache -> InstallPlan -> IO ()
realizeInstallPlan cacheDir addSourceCache (InstallPlan reusable missing) = do
  packageConfigs >>= void . initSandbox [] . map cachedPackageConfig
  where
    packageConfigs :: IO [CachedPackage]
    packageConfigs
      | null missing = return reusable
      | otherwise = populateCache cacheDir addSourceCache missing reusable
