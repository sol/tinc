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

import           Control.Monad.Catch
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import           System.Directory hiding (getDirectoryContents, withCurrentDirectory)
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
import qualified Tinc.AddSource as AddSource
import           Tinc.Package
import           Tinc.Process
import           Tinc.Sandbox
import           Tinc.AddSource
import           Tinc.Facts
import           Tinc.Types
import qualified Tinc.Nix as Nix
import           Tinc.RecentCheck
import           Util

installDependencies :: Bool -> Facts -> IO ()
installDependencies dryRun facts@Facts{..} = do
  solveDependencies facts >>= if factsUseNix
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
          mapM_ (putStrLn . ("Reusing " ++) . showPackageDetailed) (map cachedPackageName reusable)
          mapM_ (putStrLn . ("Installing " ++) . showPackageDetailed) missing
    doNix =
          tee printInstallPlan
      >=> unless dryRun . (
          tee writeFreezeFile  -- Write the freeze file before generating the nix expressions, so that our recency check works properly
      >=> Nix.createDerivations facts
          )
      where
        printInstallPlan :: [Package] -> IO ()
        printInstallPlan packages = do
          mapM_ (putStrLn . ("Using " ++) . showPackageDetailed) packages

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

solveDependencies :: Facts -> IO [Package]
solveDependencies facts@Facts{..} = do
  additionalDeps <- getAdditionalDependencies
  addSourceDependencies <- AddSource.extractAddSourceDependencies factsGitCache factsAddSourceCache additionalDeps
  cabalInstallPlan facts additionalDeps addSourceDependencies

cabalInstallPlan :: (MonadIO m, MonadMask m, Fail m, MonadProcess m) => Facts -> Hpack.Dependencies -> [AddSourceWithVersion] -> m [Package]
cabalInstallPlan facts@Facts{..} additionalDeps addSourceDependenciesWithVersions = withSystemTempDirectory "tinc" $ \dir -> do
  liftIO $ copyFreezeFile dir
  cabalFile <- liftIO (generateCabalFile additionalDeps)
  constraints <- liftIO (readFreezeFile addSourceDependencies)
  withCurrentDirectory dir $ do
    liftIO $ uncurry writeFile cabalFile
    _ <- initSandbox (map (addSourcePath factsAddSourceCache) addSourceDependencies) []
    installPlan <- cabalDryInstall facts args constraints
    return $ markAddSourceDependencies installPlan
  where
    addSourceDependencies = map fst addSourceDependenciesWithVersions
    addSourceConstraints = map addSourceConstraint addSourceDependenciesWithVersions
    args = ["--only-dependencies", "--enable-tests"] ++ addSourceConstraints
    markAddSourceDependencies = map addAddSourceHash
    addAddSourceHash :: Package -> Package
    addAddSourceHash p@(Package name version) = case lookup name addSourceHashes of
      Just rev -> Package name version {versionAddSourceHash = Just rev}
      Nothing -> p
    addSourceHashes = [(name, rev) | AddSource name rev <- addSourceDependencies]

cabalDryInstall :: (MonadIO m, Fail m, MonadProcess m, MonadCatch m) => Facts -> [String] -> [Constraint] -> m [Package]
cabalDryInstall facts args constraints = go >>= parseInstallPlan
  where
    install xs = uncurry readProcessM (cabal facts ("install" : "--dry-run" : xs)) ""

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

generateCabalFile :: Hpack.Dependencies -> IO (FilePath, String)
generateCabalFile additionalDeps = do
  hasHpackConfig <- Hpack.doesConfigExist
  cabalFiles <- getCabalFiles "."
  case cabalFiles of
    _ | hasHpackConfig -> renderHpack
    [] | not (additionalDeps == mempty) -> return generated
    [cabalFile] -> reuseExisting cabalFile
    [] -> die "No cabal file found."
    _ -> die "Multiple cabal files found."
  where
    renderHpack :: IO (FilePath, String)
    renderHpack = Hpack.render <$> Hpack.readConfig additionalDeps

    generated :: (FilePath, String)
    generated = Hpack.render (Hpack.mkPackage additionalDeps)

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
