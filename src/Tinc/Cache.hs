{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Tinc.Cache (
  Sandbox
, cabalSandboxDirectory

, findPackageDb

, PackageConfig
, listPackageConfigs

, Cache(..)
, PackageLocation(..)
, readCache

, GitRevision(..)

#ifdef TEST
, readPackageGraph
, packageFromPackageConfig
, readGitRevisions
, addRevisions
, lookupSandboxes
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Compat
import           Control.Monad.IO.Class
import           Data.Aeson.Types
import qualified Data.ByteString as B
import           Data.List.Compat
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Yaml
import           GHC.Generics
import           System.Directory
import           System.FilePath

import           Tinc.Fail
import           Tinc.GhcInfo
import           Tinc.GhcPkg
import           Tinc.Package
import           Tinc.PackageGraph
import           Tinc.Types
import           Util

data Sandbox

cabalSandboxDirectory :: FilePath
cabalSandboxDirectory = ".cabal-sandbox"

data PackageConfig

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
  sandboxes <- lookupSandboxes cacheDir
  cache <- forM sandboxes $ \ sandbox -> do
    packageDbPath <- findPackageDb sandbox
    readPackageGraph globalPackages (ghcInfoGlobalPackageDb ghcInfo) packageDbPath
  return (Cache globalPackages cache)

findPackageDb :: (MonadIO m, Fail m) => Path Sandbox -> m (Path PackageDb)
findPackageDb sandbox = do
  xs <- liftIO $ getDirectoryContents sandboxDir
  case listToMaybe (filter isPackageDb xs) of
    Just p -> liftIO $ Path <$> canonicalizePath (sandboxDir </> p)
    Nothing -> dieLoc __FILE__ ("No package database found in " ++ show sandboxDir)
  where
    sandboxDir = path sandbox </> cabalSandboxDirectory

isPackageDb :: FilePath -> Bool
isPackageDb = ("-packages.conf.d" `isSuffixOf`)

lookupSandboxes :: Path CacheDir -> IO [Path Sandbox]
lookupSandboxes (Path cacheDir) = map Path <$> listDirectories cacheDir
