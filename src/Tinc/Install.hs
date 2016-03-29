{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Tinc.Install (
  installDependencies
, cabalDryInstall_
, cabalInstallPlan_
#ifdef TEST
, cabalInstallPlan
, cabalDryInstall
, copyFreezeFile
, generateCabalFile
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Catch hiding (throwM, try)
import qualified Control.Monad.Catch as Catch
import           Control.Monad.Compat
import           Control.Monad.IO.Class
import           Data.List.Compat
import           System.IO.Temp
import           System.FilePath
import           Control.Exception (IOException)

import qualified Hpack.Config as Hpack
import           Tinc.Cabal
import           Tinc.Env as Env
import           Tinc.RunEnv
import           Tinc.Cache
import           Tinc.Config
import           Tinc.Fail
import           Tinc.Freeze
import           Tinc.GhcInfo
import qualified Tinc.Hpack as Hpack
import           Tinc.Package
import           Tinc.Process
import           Tinc.Sandbox as Sandbox
import           Tinc.Facts
import           Tinc.Types
import qualified Tinc.Nix as Nix
import           Util

installDependencies :: Bool -> Facts -> IO ()
installDependencies dryRun facts@Facts{..} = do
  solveDependencies facts factsAddSourceCache >>= if factsUseNix
  then doNix
  else doCabal
  where
    doCabal =
          createInstallPlan factsGhcInfo factsCache
      >=> tee printInstallPlan
      >=> unless dryRun . realizeInstallPlan factsCache factsAddSourceCache
      where
        printInstallPlan :: InstallPlan -> IO ()
        printInstallPlan (InstallPlan reusable missing) = do
          mapM_ (putStrLn . ("Reusing " ++) . showPackage) (map cachedPackageName reusable)
          mapM_ (putStrLn . ("Installing " ++) . showPackage) missing
    doNix =
          tee printInstallPlan
      >=> unless dryRun . Nix.createDerivations factsAddSourceCache factsNixCache
      where
        printInstallPlan :: [Package] -> IO ()
        printInstallPlan packages = do
          mapM_ (putStrLn . ("Installing " ++) . showPackage) packages

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

solveDependencies :: Facts -> Path AddSourceCache -> IO [Package]
solveDependencies facts addSourceCache = do
  additionalDeps <- getAdditionalDependencies
  addSourceDependencies <- Hpack.extractAddSourceDependencies addSourceCache additionalDeps
  run facts $ cabalInstallPlan_ additionalDeps addSourceCache addSourceDependencies

cabalInstallPlan :: (MonadIO m, MonadMask m, Fail m, Process m) => Facts -> [Hpack.Dependency] -> Path AddSourceCache -> [AddSource] -> m [Package]
cabalInstallPlan facts additionalDeps addSourceCache addSourceDependencies = System.IO.Temp.withSystemTempDirectory "tinc" $ \dir -> do
  liftIO $ run facts (copyFreezeFile dir)
  cabalFile <- liftIO (generateCabalFile additionalDeps)
  constraints <- liftIO readFreezeFile
  Util.withCurrentDirectory dir $ do
    liftIO $ uncurry writeFile cabalFile
    _ <- Sandbox.initSandbox (map (addSourcePath addSourceCache) addSourceDependencies) []
    map addAddSourceHash <$> do
      -- liftIO (run facts (cabalDryInstall_ ["--only-dependencies", "--enable-tests"] constraints)) >>= parseInstallPlan
      cabalDryInstall facts ["--only-dependencies", "--enable-tests"] constraints
  where
    addAddSourceHash :: Package -> Package
    addAddSourceHash p@(Package name version) = case lookup name addSourceHashes of
      Just rev -> Package name version {versionAddSourceHash = Just rev}
      Nothing -> p

    addSourceHashes = [(name, rev) | AddSource name rev <- addSourceDependencies]

cabalInstallPlan_ :: [Hpack.Dependency] -> Path AddSourceCache -> [AddSource] -> Tinc [Package]
cabalInstallPlan_ additionalDeps addSourceCache addSourceDependencies = Env.withSystemTempDirectory $ \dir -> do
  copyFreezeFile dir
  cabalFile <- undefined $ generateCabalFile additionalDeps
  constraints <- undefined $ readFreezeFile
  Env.withCurrentDirectory dir $ do
    uncurry (undefined writeFile) cabalFile >> return ()
    _ <- Env.initSandbox (map (addSourcePath addSourceCache) addSourceDependencies) []
    map addAddSourceHash <$> do
      cabalDryInstall_ ["--only-dependencies", "--enable-tests"] constraints >>= parseInstallPlan
  where
    addAddSourceHash :: Package -> Package
    addAddSourceHash p@(Package name version) = case lookup name addSourceHashes of
      Just rev -> Package name version {versionAddSourceHash = Just rev}
      Nothing -> p

    addSourceHashes = [(name, rev) | AddSource name rev <- addSourceDependencies]

cabalDryInstall :: (MonadIO m, Fail m, Process m, MonadCatch m) => Facts -> [String] -> [String] -> m [Package]
cabalDryInstall facts args constraints = go >>= parseInstallPlan
  where
    install xs = uncurry readProcess (cabal facts ("install" : "--dry-run" : xs)) ""

    go = do
      r <- Catch.try $ install (args ++ constraints)
      case r of
        Left _ | (not . null) constraints -> install args
        Left err -> Catch.throwM (err :: IOException)
        Right s -> return s

cabalDryInstall_ :: [String] -> [String] -> Tinc String
cabalDryInstall_ args constraints = go -- >>= parseInstallPlan
  where
    install :: [String] -> Tinc String
    install xs = readCabal ("install" : "--dry-run" : xs)

    go :: Tinc String
    go = do
      r <- try $ install (args ++ constraints)
      case r of
        Left _ | (not . null) constraints -> install args
        Left err -> throwM (err :: IOException)
        Right s -> return s

copyFreezeFile :: FilePath -> Tinc ()
copyFreezeFile dst = copyFileIfExists cabalFreezeFile (dst </> cabalFreezeFile)
  where
    cabalFreezeFile = "cabal.config"

generateCabalFile :: [Hpack.Dependency] -> IO (FilePath, String)
generateCabalFile additionalDeps = do
  hasHpackConfig <- Hpack.doesConfigExist
  cabalFiles <- filter (".cabal" `isSuffixOf`) <$> getDirectoryContents "."
  case cabalFiles of
    _ | hasHpackConfig -> renderHpack
    [] | not (null additionalDeps) -> generate
    [cabalFile] -> reuseExisting cabalFile
    [] -> die "No cabal file found."
    _ -> die "Multiple cabal files found."
  where
    renderHpack :: IO (FilePath, String)
    renderHpack = Hpack.render <$> Hpack.readConfig additionalDeps

    generate :: IO (FilePath, String)
    generate = do
      let package = Hpack.mkPackage additionalDeps
      return (Hpack.render package)

    reuseExisting :: FilePath -> IO (FilePath, String)
    reuseExisting file = (,) file <$> readFile file

realizeInstallPlan :: Path CacheDir -> Path AddSourceCache -> InstallPlan -> IO ()
realizeInstallPlan cacheDir addSourceCache (InstallPlan reusable missing) = do
  packages <- populateCache cacheDir addSourceCache missing reusable
  void . Sandbox.initSandbox [] $ map cachedPackageConfig packages
  mapM cachedExecutables packages >>= mapM_ linkExecutable . concat
  writeFreezeFile (missing ++ map cachedPackageName reusable)
  where
    linkExecutable :: FilePath -> IO ()
    linkExecutable name = linkFile name cabalSandboxBinDirectory
