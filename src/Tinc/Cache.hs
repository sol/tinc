{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Tinc.Cache (
  Cache(..)
, PackageLocation(..)
, readCache
, populateCache

#ifdef TEST
, GitRevision(..)
, listPackageConfigs
, readPackageGraph
, packageFromPackageConfig
, readGitRevisions
, addRevisions
, listSandboxes
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Catch
import           Control.Monad.Compat
import           Control.Monad.IO.Class
import           Data.Aeson.Types
import qualified Data.ByteString as B
import           Data.List.Compat
import qualified Data.Map as Map
import           Data.Yaml
import           GHC.Generics
import           System.Directory
import           System.FilePath
import           System.IO.Temp

import           Tinc.Fail
import           Tinc.GhcInfo
import           Tinc.GhcPkg
import           Tinc.Git
import           Tinc.Package
import           Tinc.PackageGraph
import           Tinc.Process
import           Tinc.Sandbox
import           Tinc.Types
import           Util

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
  fromDot (globalValues ++ values) dot >>= liftIO . addRevisions packageDb

data GitRevision = GitRevision {
  gitRevisionName :: String
, gitRevisionRevision :: String
} deriving (Eq, Ord, Show, Generic)

gitRevisionJsonOptions :: Options
gitRevisionJsonOptions = defaultOptions{fieldLabelModifier = camelTo '-' . drop (length "GitRevision")}

instance FromJSON GitRevision where
  parseJSON = genericParseJSON gitRevisionJsonOptions
instance ToJSON GitRevision where
  toJSON = genericToJSON gitRevisionJsonOptions

readGitRevisions :: Path PackageDb -> IO [GitRevision]
readGitRevisions packageDb = do
  let file = path packageDb </> "git-revisions.yaml"
  exists <- doesFileExist file
  if exists
    then B.readFile file >>= either (dieLoc __FILE__) return . decodeEither
    else return []

addRevision :: [GitRevision] -> Package -> PackageLocation -> Package
addRevision revisions package location = case location of
  PackageConfig _ -> maybe package (\ revision -> setGitRevision revision package) (Map.lookup (packageName package) revisionMap)
  GlobalPackage -> package
  where
    revisionMap :: Map.Map String String
    revisionMap = Map.fromList (map (\ (GitRevision name revision) -> (name, revision)) revisions)

addRevisions :: Path PackageDb -> PackageGraph PackageLocation -> IO (PackageGraph PackageLocation)
addRevisions packageDb graph = do
  revisions <- readGitRevisions packageDb
  return $ mapIndex (addRevision revisions) graph

readCache :: GhcInfo -> Path CacheDir -> IO Cache
readCache ghcInfo cacheDir = do
  globalPackages <- listGlobalPackages
  sandboxes <- listSandboxes cacheDir
  cache <- forM sandboxes $ \ sandbox -> do
    packageDbPath <- findPackageDb sandbox
    readPackageGraph globalPackages (ghcInfoGlobalPackageDb ghcInfo) packageDbPath
  return (Cache globalPackages cache)

validMarker :: FilePath
validMarker = "tinc.valid"

listSandboxes :: Path CacheDir -> IO [Path Sandbox]
listSandboxes (Path cacheDir) = map Path <$> listEntries
  where
    isValidCacheEntry :: FilePath -> IO Bool
    isValidCacheEntry p = doesFileExist (p </> validMarker)

    listEntries :: IO [FilePath]
    listEntries = listDirectories cacheDir >>= filterM isValidCacheEntry

populateCache :: forall m . (MonadIO m, MonadMask m, Fail m, Process m) =>
  Path CacheDir -> Path GitCache -> [Package] -> [(Package, Path PackageConfig)] -> m [Path PackageConfig]
populateCache cacheDir gitCache missing reusable = do
  basename <- takeBaseName <$> liftIO getCurrentDirectory
  sandbox <- liftIO $ createTempDirectory (path cacheDir) (basename ++ "-")
  populate sandbox
  list sandbox
  where
    installPlan :: [Package]
    installPlan = missing ++ map fst reusable

    populate sandbox = do
      withCurrentDirectory sandbox $ do
        packageDb <- initSandbox (map gitRevisionToPath gitRevisions) (map snd reusable)
        writeGitRevisions packageDb
        liftIO $ writeFile validMarker ""
        callProcess "cabal" ("install" : map showPackage installPlan)

    gitRevisionToPath :: GitRevision -> Path CachedGitDependency
    gitRevisionToPath GitRevision{..} = revisionToPath gitCache gitRevisionName gitRevisionRevision

    list :: FilePath -> m [Path PackageConfig]
    list sandbox = do
      sourcePackageDb <- findPackageDb (Path sandbox)
      map snd <$> listPackageConfigs sourcePackageDb

    writeGitRevisions packageDb
      | null gitRevisions = return ()
      | otherwise = do
          liftIO $ encodeFile (path packageDb </> "git-revisions.yaml") gitRevisions
          recache packageDb

    gitRevisions = [GitRevision name revision | Package name (Version _ (Just revision)) <- missing]
