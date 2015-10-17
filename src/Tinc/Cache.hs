{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Tinc.Cache (
  Cache
, CachedPackage(..)
, readCache
, findReusablePackages
, populateCache

#ifdef TEST
, PackageLocation(..)
, listPackages
, readPackageGraph
, packageFromPackageConfig
, readAddSourceHashes
, addAddSourceHashes
, listSandboxes
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Catch
import           Control.Monad.Compat
import           Control.Monad.IO.Class
import qualified Data.ByteString as B
import           Data.List.Compat
import qualified Data.Map as Map
import           Data.Yaml
import           System.Directory hiding (getDirectoryContents)
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
import           Tinc.Types
import           Util

data CachedPackage = CachedPackage {
  cachedPackageName :: Package
, cachedPackageConfig :: Path PackageConfig
} deriving (Eq, Show)

findReusablePackages :: Cache -> [Package] -> [CachedPackage]
findReusablePackages (Cache globalPackages packageGraphs) installPlan = reusablePackages
  where
    reusablePackages :: [CachedPackage]
    reusablePackages = nubBy ((==) `on` cachedPackageName) (concatMap findReusable packageGraphs)

    findReusable :: PackageGraph PackageLocation -> [CachedPackage]
    findReusable cacheGraph =
      [CachedPackage p c | (p, PackageConfig c)  <- calculateReusablePackages packages cacheGraph]
      where
        packages = nubBy ((==) `on` packageName) (installPlan ++ globalPackages)

listPackages :: MonadIO m => Path PackageDb -> m [CachedPackage]
listPackages p = do
  packageConfigs <- liftIO $ filter (".conf" `isSuffixOf`) <$> getDirectoryContents (path p)
  absolutePackageConfigs <- liftIO . mapM canonicalizePath $ map (path p </>) packageConfigs
  let packages = map packageFromPackageConfig packageConfigs
  return (zipWith CachedPackage packages (map Path absolutePackageConfigs))

packageFromPackageConfig :: FilePath -> Package
packageFromPackageConfig = parsePackage . reverse . drop 1 . dropWhile (/= '-') . reverse

data Cache = Cache {
  _cacheGlobalPackages :: [Package]
, _cachePackageGraphs :: [PackageGraph PackageLocation]
}

data PackageLocation = GlobalPackage | PackageConfig (Path PackageConfig)
  deriving (Eq, Ord, Show)

readPackageGraph :: (MonadIO m, Fail m, GhcPkg m) => [Package] -> Path PackageDb -> Path PackageDb -> m (PackageGraph PackageLocation)
readPackageGraph globalPackages globalPackageDb packageDb = do
  packageConfigs <- liftIO $ listPackages packageDb
  let globalValues = map (, GlobalPackage) globalPackages
  let values = toValues packageConfigs
  dot <- readGhcPkg [globalPackageDb, packageDb] ["dot"]
  fromDot (globalValues ++ values) dot >>= liftIO . addAddSourceHashes packageDb
  where
    toValues :: [CachedPackage] -> [(Package, PackageLocation)]
    toValues packages = [(p, PackageConfig c) | CachedPackage p c <- packages]

addSourceHashesFile :: FilePath
addSourceHashesFile = "add-source.yaml"

readAddSourceHashes :: Path PackageDb -> IO [AddSource]
readAddSourceHashes packageDb = do
  let file = path packageDb </> addSourceHashesFile
  exists <- doesFileExist file
  if exists
    then B.readFile file >>= either (dieLoc __FILE__) return . decodeEither
    else return []

addAddSourceHash :: Map.Map String String -> Package -> PackageLocation -> Package
addAddSourceHash hashes package location = case location of
  PackageConfig _ -> maybe package (\ hash -> setAddSourceHash hash package) (Map.lookup (packageName package) hashes)
  GlobalPackage -> package

addAddSourceHashes :: Path PackageDb -> PackageGraph PackageLocation -> IO (PackageGraph PackageLocation)
addAddSourceHashes packageDb graph = do
  hashes <- mkMap <$> readAddSourceHashes packageDb
  return $ mapIndex (addAddSourceHash hashes) graph
  where
    mkMap :: [AddSource] -> Map.Map String String
    mkMap hashes = Map.fromList (map (\ (AddSource name hash) -> (name, hash)) hashes)

readCache :: GhcInfo -> Path CacheDir -> IO Cache
readCache ghcInfo cacheDir = do
  globalPackages <- listGlobalPackages
  sandboxes <- listSandboxes cacheDir
  cache <- forM sandboxes $ \ sandbox -> do
    packageDbPath <- findPackageDb sandbox
    readPackageGraph globalPackages (ghcInfoGlobalPackageDb ghcInfo) packageDbPath
  return (Cache globalPackages cache)

validMarker :: FilePath
validMarker = "tinc.valid.v2"

listSandboxes :: Path CacheDir -> IO [Path Sandbox]
listSandboxes (Path cacheDir) = map Path <$> listEntries
  where
    isValidCacheEntry :: FilePath -> IO Bool
    isValidCacheEntry p = doesFileExist (p </> validMarker)

    listEntries :: IO [FilePath]
    listEntries = listDirectories cacheDir >>= filterM isValidCacheEntry

populateCache :: forall m . (MonadIO m, MonadMask m, Fail m, Process m) =>
  Path CacheDir -> Path AddSourceCache -> [Package] -> [CachedPackage] -> m [CachedPackage]
populateCache cacheDir addSourceCache missing reusable
  | null missing = return reusable
  | otherwise = do
      basename <- takeBaseName <$> liftIO getCurrentDirectory
      sandbox <- liftIO $ createTempDirectory (path cacheDir) (basename ++ "-")
      populate sandbox
      list sandbox
  where
    installPlan :: [Package]
    installPlan = missing ++ map cachedPackageName reusable

    populate sandbox = do
      withCurrentDirectory sandbox $ do
        packageDb <- initSandbox (map (addSourcePath addSourceCache) addSourceHashes) (map cachedPackageConfig reusable)
        writeAddSourceHashes packageDb
        liftIO $ writeFile validMarker ""
        callProcess "cabal" ("install" : map showPackage installPlan)

    list :: FilePath -> m [CachedPackage]
    list sandbox = do
      sourcePackageDb <- findPackageDb (Path sandbox)
      listPackages sourcePackageDb

    writeAddSourceHashes packageDb
      | null addSourceHashes = return ()
      | otherwise = do
          liftIO $ encodeFile (path packageDb </> addSourceHashesFile) addSourceHashes
          recache packageDb

    addSourceHashes = [AddSource name hash | Package name (Version _ (Just hash)) <- (missing ++ map cachedPackageName reusable)]
