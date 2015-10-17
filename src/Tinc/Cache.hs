{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Tinc.Cache (
  Cache
, readCache
, findReusablePackages
, populateCache

#ifdef TEST
, PackageLocation(..)
, listPackageConfigs
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
import           System.Directory
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

listPackageConfigs :: MonadIO m => Path PackageDb -> m [(Package, Path PackageConfig)]
listPackageConfigs p = do
  packageConfigs <- liftIO $ filter (".conf" `isSuffixOf`) <$> getDirectoryContents (path p)
  let packages = map packageFromPackageConfig packageConfigs
      absolutePackageConfigs = map (Path . (path p </>)) packageConfigs
  return (zip packages absolutePackageConfigs)

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
  packageConfigs <- liftIO $ listPackageConfigs packageDb
  let globalValues = map (, GlobalPackage) globalPackages
  let values = map (fmap PackageConfig) packageConfigs
  dot <- readGhcPkg [globalPackageDb, packageDb] ["dot"]
  fromDot (globalValues ++ values) dot >>= liftIO . addAddSourceHashes packageDb

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
  Path CacheDir -> Path AddSourceCache -> [Package] -> [(Package, Path PackageConfig)] -> m [Path PackageConfig]
populateCache cacheDir addSourceCache missing reusable = do
  basename <- takeBaseName <$> liftIO getCurrentDirectory
  sandbox <- liftIO $ createTempDirectory (path cacheDir) (basename ++ "-")
  populate sandbox
  list sandbox
  where
    installPlan :: [Package]
    installPlan = missing ++ map fst reusable

    populate sandbox = do
      withCurrentDirectory sandbox $ do
        packageDb <- initSandbox (map (addSourcePath addSourceCache) addSourceHashes) (map snd reusable)
        writeAddSourceHashes packageDb
        liftIO $ writeFile validMarker ""
        callProcess "cabal" ("install" : map showPackage installPlan)

    list :: FilePath -> m [Path PackageConfig]
    list sandbox = do
      sourcePackageDb <- findPackageDb (Path sandbox)
      map snd <$> listPackageConfigs sourcePackageDb

    writeAddSourceHashes packageDb
      | null addSourceHashes = return ()
      | otherwise = do
          liftIO $ encodeFile (path packageDb </> addSourceHashesFile) addSourceHashes
          recache packageDb

    addSourceHashes = [AddSource name hash | Package name (Version _ (Just hash)) <- (missing ++ map fst reusable)]
