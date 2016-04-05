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
, cloneRemoteRepoCache
, listRemoteRepos
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
import           Tinc.Cabal
import           Tinc.Cache
import           Tinc.Config
import           Tinc.Fail
import           Tinc.Freeze
import           Tinc.GhcInfo
import qualified Tinc.Hpack as Hpack
import           Tinc.Package
import           Tinc.Process
import           Tinc.Sandbox
import           Tinc.Facts
import           Tinc.Types
import qualified Tinc.Nix as Nix
import           Tinc.RecentCheck
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
      >=> unless dryRun . (
          realizeInstallPlan factsCache factsAddSourceCache
      >=> \() -> markRecent factsGhcInfo
          )
      where
        printInstallPlan :: InstallPlan -> IO ()
        printInstallPlan (InstallPlan reusable missing) = do
          mapM_ (putStrLn . ("Reusing " ++) . showPackage) (map cachedPackageName reusable)
          mapM_ (putStrLn . ("Installing " ++) . showPackage) missing
    doNix =
          tee printInstallPlan
      >=> unless dryRun . (
          tee writeFreezeFile  -- Write the freeze file before generating the nix expressions, so that our recency check works properly
      >=> Nix.createDerivations facts
          )
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
  withSystemTempDirectory "tinc-remote-repo-cache" $ \(Path -> remoteRepoCache) -> do
    cloneRemoteRepoCache (factsRemoteRepoCache facts) remoteRepoCache
    cabalInstallPlan facts{ factsRemoteRepoCache = remoteRepoCache } additionalDeps addSourceCache addSourceDependencies

remoteRepoTarFile :: FilePath
remoteRepoTarFile = "00-index.tar"

remoteRepoCacheFile :: FilePath
remoteRepoCacheFile = "00-index.cache"

cloneRemoteRepoCache :: Path RemoteRepoCache -> Path RemoteRepoCache -> IO ()
cloneRemoteRepoCache src (Path dst) = do
  remotes <- listRemoteRepos src
  forM_ remotes $ \remote -> do
    createDirectoryIfMissing True $ dst </> remote
    linkFile (path src </> remote </> remoteRepoTarFile) (dst </> remote </> remoteRepoTarFile)
    copyFile (path src </> remote </> remoteRepoCacheFile) (dst </> remote </> remoteRepoCacheFile)

listRemoteRepos :: Path RemoteRepoCache -> IO [FilePath]
listRemoteRepos (Path dir) = do
  remotes <- getDirectories dir
  filterM (doesFileExist . (\remote -> dir </> remote </> remoteRepoTarFile)) remotes

cabalInstallPlan :: (MonadIO m, MonadMask m, Fail m, MonadProcess m) => Facts -> [Hpack.Dependency] -> Path AddSourceCache -> [AddSource] -> m [Package]
cabalInstallPlan facts additionalDeps addSourceCache addSourceDependencies = withSystemTempDirectory "tinc" $ \dir -> do
  liftIO $ copyFreezeFile dir
  cabalFile <- liftIO (generateCabalFile additionalDeps)
  constraints <- liftIO (readFreezeFile addSourceDependencies)
  withCurrentDirectory dir $ do
    liftIO $ uncurry writeFile cabalFile
    _ <- initSandbox (map (addSourcePath addSourceCache) addSourceDependencies) []
    map addAddSourceHash <$> cabalDryInstall facts ["--only-dependencies", "--enable-tests"] constraints
  where
    addAddSourceHash :: Package -> Package
    addAddSourceHash p@(Package name version) = case lookup name addSourceHashes of
      Just rev -> Package name version {versionAddSourceHash = Just rev}
      Nothing -> p

    addSourceHashes = [(name, rev) | AddSource name rev <- addSourceDependencies]

cabalDryInstall :: (MonadIO m, Fail m, MonadProcess m, MonadCatch m) => Facts -> [String] -> [String] -> m [Package]
cabalDryInstall facts@Facts{..} args constraints = go >>= parseInstallPlan
  where
    install xs = uncurry readProcessM (cabal facts ("--remote-repo-cache" : path factsRemoteRepoCache : "install" : "--dry-run" : xs)) ""

    go = do
      r <- try $ install (args ++ constraints)
      case r of
        Left _ | (not . null) constraints -> install args
        Left err -> throwM (err :: IOException)
        Right s -> return s

copyFreezeFile :: FilePath -> IO ()
copyFreezeFile dst = do
  exists <- doesFileExist cabalFreezeFile
  when exists $ do
    copyFile cabalFreezeFile (dst </> cabalFreezeFile)
  where
    cabalFreezeFile = "cabal.config"

generateCabalFile :: [Hpack.Dependency] -> IO (FilePath, String)
generateCabalFile additionalDeps = do
  hasHpackConfig <- Hpack.doesConfigExist
  cabalFiles <- getCabalFiles "."
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
  writeFreezeFile (missing ++ map cachedPackageName reusable) -- Write the freeze file before creating the sandbox, so that our recency check works properly
  void . initSandbox [] $ map cachedPackageConfig packages
  mapM cachedExecutables packages >>= mapM_ linkExecutable . concat
  where
    linkExecutable :: FilePath -> IO ()
    linkExecutable name = linkFile name cabalSandboxBinDirectory
