{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import           System.FilePath
import           System.IO.Temp

import qualified Hpack.Config as Hpack
import           Tinc.Cache
import           Tinc.Config
import           Tinc.Fail
import           Tinc.GhcInfo
import           Tinc.Git
import qualified Tinc.Hpack as Hpack
import           Tinc.Package
import           Tinc.PackageGraph
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
  _installPlanReusable :: [(Package, Path PackageConfig)]
, _installPlanMissing :: [Package]
} deriving (Eq, Show)

createInstallPlan :: GhcInfo -> Path CacheDir -> [Package] -> IO InstallPlan
createInstallPlan ghcInfo cacheDir installPlan = do
  cache <- readCache ghcInfo cacheDir
  let reusable = findReusablePackages cache installPlan
      missing = installPlan \\ map fst reusable
  return (InstallPlan reusable missing)

solveDependencies :: Path AddSourceCache -> IO [Package]
solveDependencies addSourceCache = do
  additionalDeps <- getAdditionalDependencies
  cachedGitDependencies <- Hpack.extractGitDependencies additionalDeps >>= mapM (clone addSourceCache)
  cachedLocalDependencies <- localDependencies addSourceCache additionalDeps
  let addSourceDependencies = cachedLocalDependencies ++ cachedGitDependencies
  cabalInstallPlan additionalDeps addSourceCache addSourceDependencies

localDependencies :: Path AddSourceCache -> [Hpack.Dependency] -> IO [AddSource]
localDependencies addSourceCache dependencies = do
  mapM local [(name, path) | Hpack.Dependency name (Just (Hpack.Local path)) <- dependencies]
  where
    local :: (String, FilePath) -> IO AddSource
    local (name, dir) = do
      withSystemTempDirectory "tinc" $ \ ((</> "package") -> tempDir) -> do
        createDirectory tempDir
        cabalSdist dir tempDir
        fp <- fingerprint tempDir
        let dst = path addSourceCache </> name </> fp
        createDirectoryIfMissing True (path addSourceCache </> name)
        cabalSdist tempDir dst
        return $ AddSource name fp

    cabalSdist :: FilePath -> FilePath -> IO ()
    cabalSdist sourceDirectory tempDir = do
      withCurrentDirectory sourceDirectory $ do
        callProcess "cabal" ["sdist", "--output-directory", tempDir]

cabalInstallPlan :: (MonadIO m, MonadMask m, Fail m, Process m) => [Hpack.Dependency] -> Path AddSourceCache -> [AddSource] -> m [Package]
cabalInstallPlan additionalDeps addSourceCache addSourceDependencies = withSystemTempDirectory "tinc" $ \dir -> do
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
  mapM_ (putStrLn . ("Reusing " ++) . showPackage) (map fst reusable)
  mapM_ (putStrLn . ("Installing " ++) . showPackage) missing

realizeInstallPlan :: Path CacheDir -> Path AddSourceCache -> InstallPlan -> IO ()
realizeInstallPlan cacheDir addSourceCache (InstallPlan reusable missing) =
  packageConfigs >>= void . initSandbox []
  where
    packageConfigs
      | null missing = return (map snd reusable)
      | otherwise = populateCache cacheDir addSourceCache missing reusable

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
