{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Tinc.Install (
  installDependencies
#ifdef TEST
, cabalInstallPlan
, cabalDryInstall
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
import           System.Directory hiding (getDirectoryContents)
import           System.IO.Temp
import           System.FilePath
import           Control.Exception (IOException)

import qualified Hpack.Config as Hpack
import           Tinc.Cache
import           Tinc.Config
import           Tinc.Fail
import           Tinc.Freeze
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
  addSourceDependencies <- Hpack.extractAddSourceDependencies addSourceCache additionalDeps
  cabalInstallPlan additionalDeps addSourceCache addSourceDependencies

cabalInstallPlan :: (MonadIO m, MonadMask m, Fail m, Process m) => [Hpack.Dependency] -> Path AddSourceCache -> [AddSource] -> m [Package]
cabalInstallPlan additionalDeps addSourceCache addSourceDependencies = withSystemTempDirectory "tinc" $ \dir -> do
  liftIO $ copyFreezeFile dir
  cabalFile <- liftIO (generateCabalFile additionalDeps)
  constraints <- liftIO readFreezeFile
  withCurrentDirectory dir $ do
    liftIO $ uncurry writeFile cabalFile
    _ <- initSandbox (map (addSourcePath addSourceCache) addSourceDependencies) []
    map addAddSourceHash <$> cabalDryInstall ["--only-dependencies", "--enable-tests"] constraints
  where
    addAddSourceHash :: Package -> Package
    addAddSourceHash p@(Package name version) = case lookup name addSourceHashes of
      Just rev -> Package name version {versionAddSourceHash = Just rev}
      Nothing -> p

    addSourceHashes = [(name, rev) | AddSource name rev <- addSourceDependencies]

cabalDryInstall :: (Fail m, Process m, MonadCatch m) => [String] -> [String] -> m [Package]
cabalDryInstall args constraints = go >>= parseInstallPlan
  where
    install xs = readProcess "cabal" ("install" : "--dry-run" : xs) ""

    go = do
      r <- try $ install (args ++ constraints)
      case r of
        Left _ | (not . null) constraints -> install args
        Left err -> throwM (err :: IOException)
        Right s -> return s

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
  packages <- populateCache cacheDir addSourceCache missing reusable
  void . initSandbox [] $ map cachedPackageConfig packages
  mapM cachedExecutables packages >>= mapM_ linkExecutable . concat
  writeFreezeFile (missing ++ map cachedPackageName reusable)
  where
    linkExecutable :: FilePath -> IO ()
    linkExecutable name = linkFile name cabalSandboxBinDirectory
