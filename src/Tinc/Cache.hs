{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Tinc.Cache (
  Cache
, CachedPackage(..)
, readCache
, findReusablePackages
, cachedExecutables
, populateCache

#ifdef TEST
, PopulateCacheAction(..)
, populateCacheAction

, PackageLocation(..)
, readPackageGraph
, readAddSourceHashes
, addAddSourceHashes
, listSandboxes
#endif
) where

import           Control.Monad.Catch
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import qualified Data.Map as Map
import           Data.Yaml
import           System.Directory hiding (getDirectoryContents, withCurrentDirectory)
import           System.FilePath
import           System.IO.Temp
import           Data.Function

import           Tinc.Fail
import           Tinc.GhcInfo
import           Tinc.GhcPkg
import           Tinc.Package
import           Tinc.PackageGraph
import           Tinc.Process
import           Tinc.Sandbox
import           Tinc.SourceDependency
import           Tinc.Types
import           Util

data CachedPackage = CachedPackage {
  cachedPackageName :: Package
, cachedPackageConfig :: Path PackageConfig
} deriving (Eq, Show)

cachedExecutables :: CachedPackage -> IO [FilePath]
cachedExecutables (CachedPackage package (Path config)) = do
  exists <- doesDirectoryExist binDir
  if exists
    then listDirectoryContents binDir >>= mapM canonicalizePath
    else return []
  where
    binDir = dropFileName config </> ".." </> "bin" </> showPackage package

findReusablePackages :: Cache -> [Package] -> [CachedPackage]
findReusablePackages (Cache globalPackages packageGraphs) installPlan = reusablePackages
  where
    reusablePackages :: [CachedPackage]
    reusablePackages = nubBy ((==) `on` cachedPackageName) (concatMap findReusable packageGraphs)

    findReusable :: PackageGraph PackageLocation -> [CachedPackage]
    findReusable packageGraph =
      [CachedPackage p c | (p, PackageConfig c)  <- calculateReusablePackages packages packageGraph]
      where
        packages = nubBy ((==) `on` packageName) (installPlan ++ map fromSimplePackage globalPackages)

data Cache = Cache {
  _cacheGlobalPackages :: [SimplePackage]
, _cachePackageGraphs :: [PackageGraph PackageLocation]
}

data PackageLocation = GlobalPackage | PackageConfig (Path PackageConfig)
  deriving (Eq, Ord, Show)

fromSimplePackage :: SimplePackage -> Package
fromSimplePackage (SimplePackage name version) = Package name (Version version Nothing)

readPackageGraph :: (MonadIO m, Fail m, GhcPkg m) => [SimplePackage] -> Path PackageDb -> Path PackageDb -> m (PackageGraph PackageLocation)
readPackageGraph globalPackages globalPackageDb packageDb = do
  packageConfigs <- liftIO $ cachedListPackages packageDb
  let globalValues = map (, GlobalPackage) globalPackages
  let values = map (fmap PackageConfig) packageConfigs
  dot <- readDotFile
  fromDot (globalValues ++ values) dot >>= liftIO . addAddSourceHashes packageDb
  where
    dotFile = path packageDb </> "packages.dot"
    readDotFile = do
      cachedIOAfter (liftIO $ touchPackageCache packageDb) dotFile $ do
        readGhcPkg [globalPackageDb, packageDb] ["dot"]

addSourceHashesFile :: FilePath
addSourceHashesFile = "add-source.yaml"

readAddSourceHashes :: Path PackageDb -> IO [SourceDependency]
readAddSourceHashes packageDb = do
  let file = path packageDb </> addSourceHashesFile
  exists <- doesFileExist file
  if exists
    then decodeFileEither file >>= either (dieLoc . show) return
    else return []

writeAddSourceHashes :: Path PackageDb -> [SourceDependency] -> IO ()
writeAddSourceHashes packageDb addSourceHashes
  | null addSourceHashes = return ()
  | otherwise = do
      encodeFile (path packageDb </> addSourceHashesFile) addSourceHashes
      touchPackageCache packageDb

addAddSourceHash :: Map.Map String String -> SimplePackage -> PackageLocation -> Package
addAddSourceHash hashes (SimplePackage name version) location = case location of
  PackageConfig _ -> maybe package (\ hash -> Package name (Version version $ Just hash)) (Map.lookup (packageName package) hashes)
  GlobalPackage -> package
  where
    package = Package name (Version version Nothing)

addAddSourceHashes :: Path PackageDb -> SimplePackageGraph PackageLocation -> IO (PackageGraph PackageLocation)
addAddSourceHashes packageDb graph = do
  hashes <- mkMap <$> readAddSourceHashes packageDb
  return $ mapIndex (addAddSourceHash hashes) graph
  where
    mkMap :: [SourceDependency] -> Map.Map String String
    mkMap hashes = Map.fromList (map (\ (SourceDependency name hash) -> (name, hash)) hashes)

readCache :: GhcInfo -> Path CacheDir -> IO Cache
readCache ghcInfo cacheDir = do
  globalPackages <- listGlobalPackages
  sandboxes <- listSandboxes cacheDir
  cache <- forM sandboxes $ \ sandbox -> do
    packageDbPath <- findPackageDb sandbox
    readPackageGraph globalPackages (ghcInfoGlobalPackageDb ghcInfo) packageDbPath
  return (Cache globalPackages cache)

validMarker :: FilePath
validMarker = "tinc.valid.v3"

listSandboxes :: Path CacheDir -> IO [Path Sandbox]
listSandboxes (Path cacheDir) = map Path <$> listEntries
  where
    isValidCacheEntry :: FilePath -> IO Bool
    isValidCacheEntry p = doesFileExist (p </> validMarker)

    listEntries :: IO [FilePath]
    listEntries = listDirectories cacheDir >>= filterM isValidCacheEntry

data PopulateCacheAction = PopulateCacheAction {
  populateCacheActionInstallPlan :: [Package]
, populateCacheActionAddSource :: [Path SourceDependency]
, populateCacheActionWriteAddSourceHashes :: [SourceDependency]
} deriving (Eq, Show)

populateCacheAction :: Path SourceDependencyCache -> [Package] -> [CachedPackage] -> Either [CachedPackage] PopulateCacheAction
populateCacheAction sourceDependencyCache missing reusable
  | null missing = Left reusable
  | otherwise = Right PopulateCacheAction {
    populateCacheActionInstallPlan = installPlan
  , populateCacheActionAddSource = addSource
  , populateCacheActionWriteAddSourceHashes = [SourceDependency name hash | Package name (Version _ (Just hash)) <- (missing ++ map cachedPackageName reusable)]
  }
  where
    installPlan :: [Package]
    installPlan = missing ++ [p | p@(Package _ (Version _ Nothing)) <- map cachedPackageName reusable]

    addSource :: [Path SourceDependency]
    addSource = map (sourceDependencyPath sourceDependencyCache) [SourceDependency name hash | Package name (Version _ (Just hash)) <- missing]

populateCache :: (MonadIO m, MonadMask m, Fail m, MonadProcess m) => Path CacheDir -> Path SourceDependencyCache -> [Package] -> [CachedPackage] -> m [CachedPackage]
populateCache cacheDir sourceDependencyCache missing reusable = either return populate (populateCacheAction sourceDependencyCache missing reusable)
  where
    populate PopulateCacheAction{..} = do
      sandbox <- liftIO $ newCacheEntry cacheDir
      withCurrentDirectory sandbox $ do
        packageDb <- initSandbox populateCacheActionAddSource (map cachedPackageConfig reusable)
        liftIO $ do
          writeAddSourceHashes packageDb populateCacheActionWriteAddSourceHashes
          writeFile validMarker ""
        callProcessM "cabal" ("v1-install" : "--bindir=$prefix/bin/$pkgid" : map showPackage populateCacheActionInstallPlan)
        map (uncurry CachedPackage)
          . ignore_add_source_hashes_for_now_as_we_currently_do_not_need_them
          <$> cachedListPackages packageDb

    ignore_add_source_hashes_for_now_as_we_currently_do_not_need_them = map (\ (a, b) -> (fromSimplePackage a, b))

newCacheEntry :: Path CacheDir -> IO FilePath
newCacheEntry cacheDir = do
  basename <- takeBaseName <$> getCurrentDirectory
  createTempDirectory (path cacheDir) (basename ++ "-")
