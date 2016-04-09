{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tinc.AddSource (
  GitCache
, AddSourceCache
, AddSource(..)
, addSourcePath
, extractAddSourceDependencies
#ifdef TEST
, AddSourceDependency(..)
, Source(..)
, Ref(..)
, Rev(..)
, CachedRev(..)
, extractAddSourceDependencies_impl
, parseAddSourceDependencies
, populateAddSourceCache_impl

, gitClone_impl

, gitRefToRev_impl
, isGitRev
, checkCabalName
, determinePackageName
, findCabalFile
#endif
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Function
import           Data.List
import           Data.String
import           Distribution.Package
import           Distribution.PackageDescription hiding (Git)
import           Distribution.PackageDescription.Parse
import           Distribution.Verbosity
import           GHC.Fingerprint
import qualified Hpack.Config as Hpack
import           System.Directory hiding (getDirectoryContents)
import           System.FilePath
import           System.IO.Temp
import           Data.Aeson
import           Data.Aeson.Types
import           GHC.Generics

import           Tinc.Fail
import           Tinc.Types
import           Tinc.Process
import           Tinc.Hpack
import           Util

data GitCache
data AddSourceCache

data AddSource = AddSource {
  addSourcePackageName :: String

-- This is one of:
--  + git revision for git dependencies
--  + md5(md5(git revision), md5(subdir)) for git dependencies that specify a subdir
--  + md5 of local dependency for local dependencies
, addSourceHash :: String

} deriving (Eq, Show, Generic)

data AddSourceDependency a = AddSourceDependency {
  _addSourceDependencyName :: String
, _addSourceDependencySource :: Source a
} deriving (Eq, Show)

newtype Ref = Ref String
  deriving (Eq, Show, IsString)

newtype Rev = Rev String
  deriving (Eq, Show, IsString)

newtype CachedRev = CachedRev {unCachedRev :: String}
  deriving (Eq, Show, IsString)

data Source a = Local FilePath | Git String a (Maybe FilePath)
  deriving (Eq, Show)

addSourceJsonOptions :: Options
addSourceJsonOptions = defaultOptions{fieldLabelModifier = camelTo2 '-' . drop (length ("AddSource" :: String))}

instance FromJSON AddSource where
  parseJSON = genericParseJSON addSourceJsonOptions
instance ToJSON AddSource where
  toJSON = genericToJSON addSourceJsonOptions

addSourcePath :: Path AddSourceCache -> AddSource -> Path AddSource
addSourcePath (Path cache) (AddSource name rev) = Path $ cache </> name </> rev

extractAddSourceDependencies :: Path GitCache -> Path AddSourceCache -> [Hpack.Dependency] -> IO [AddSource]
extractAddSourceDependencies gitCache addSourceCache additionalDeps =
  parseAddSourceDependencies additionalDeps >>= extractAddSourceDependencies_impl (addSourceDependenciesFrom addSourceCache) resolveGitReferences (cacheGitRev gitCache) (populateAddSourceCache_impl gitCache addSourceCache)

extractAddSourceDependencies_impl :: AddSourceDependenciesFrom ref -> ResolveGitReferences ref rev -> CacheGitRev rev cachedRev -> PopulateAddSourceCache cachedRev -> [AddSourceDependency ref] -> IO [AddSource]
extractAddSourceDependencies_impl addSourceDependenciesFrom_ resolveGitReferences_ cacheGitRev_ populateAddSourceCache_ = go
  where
    go deps = do
      case deps of
        [] -> return []
        _ -> do
          xs <- mapM resolveGitReferences_ deps >>= mapM cacheGitRev_ >>= mapM populateAddSourceCache_
          (xs ++) <$> (mapM addSourceDependenciesFrom_ xs >>= go . concat)

type ResolveGitReferences ref rev = AddSourceDependency ref -> IO (AddSourceDependency rev)

resolveGitReferences :: ResolveGitReferences Ref Rev
resolveGitReferences (AddSourceDependency name source) = AddSourceDependency name <$> case source of
  Git url ref subdir -> Git url <$> gitRefToRev url ref <*> pure subdir
  Local dir -> return (Local dir)

type CacheGitRev rev cachedRev = AddSourceDependency rev -> IO (AddSourceDependency cachedRev)

cacheGitRev :: Path GitCache -> CacheGitRev Rev CachedRev
cacheGitRev cache (AddSourceDependency name source) = AddSourceDependency name <$> case source of
  Git url rev subdir -> Git url <$> gitClone cache url rev <*> pure subdir
  Local dir -> return (Local dir)

gitClone :: Path GitCache -> String -> Rev -> IO CachedRev
gitClone = gitClone_impl process

gitClone_impl :: Process IO -> Path GitCache -> String -> Rev -> IO CachedRev
gitClone_impl Process{..} cache url (Rev rev) = do
  createDirectoryIfMissing True (path cache)
  withTempDirectory (path cache) "tmp" $ \sandbox -> do
    let tmp = sandbox </> rev
    alreadyInCache <- doesDirectoryExist dst
    unless alreadyInCache $ do
      callProcess "git" ["clone", url, tmp]
      withCurrentDirectory tmp $ do
        callProcess "git" ["reset", "--hard", rev]
        removeDirectoryRecursive ".git"
      renameDirectory tmp dst
  return cachedRev
  where
    cachedRev = CachedRev rev
    Path dst = cachedRevPath cache cachedRev

cachedRevPath :: Path GitCache -> CachedRev -> Path CachedRev
cachedRevPath (Path cache) (CachedRev rev) = Path (cache </> rev)

type AddSourceDependenciesFrom ref = AddSource -> IO [AddSourceDependency ref]

addSourceDependenciesFrom :: Path AddSourceCache -> AddSourceDependenciesFrom Ref
addSourceDependenciesFrom addSourceCache addSource = do
  exists <- doesFileExist config
  if exists
    then Hpack.readPackageConfig config >>= either die (return . filterAddSource . Hpack.packageDependencies . snd)
    else return []
  where
    config = path (addSourcePath addSourceCache addSource) </> Hpack.packageConfig

filterAddSource :: [Hpack.Dependency] -> [AddSourceDependency Ref]
filterAddSource deps = [AddSourceDependency name (toSource addSource) | Hpack.Dependency name (Just addSource) <- deps]
  where
    toSource :: Hpack.AddSource -> Source Ref
    toSource x = case x of
      Hpack.GitRef repo ref subdir -> Git repo (Ref ref) subdir
      Hpack.Local dir -> Local dir

parseAddSourceDependencies :: [Hpack.Dependency] ->  IO [AddSourceDependency Ref]
parseAddSourceDependencies additionalDeps = do
  exists <- doesConfigExist
  packageDeps <- if exists
    then do
      pkg <- readConfig []
      return $ Hpack.packageDependencies pkg
    else return []
  let deps = nubBy ((==) `on` Hpack.dependencyName) (additionalDeps ++ packageDeps)
  return (filterAddSource deps)

type PopulateAddSourceCache cachedRev = AddSourceDependency cachedRev -> IO AddSource

populateAddSourceCache_impl :: Path GitCache -> Path AddSourceCache -> PopulateAddSourceCache CachedRev
populateAddSourceCache_impl gitCache cache dep@(AddSourceDependency name source) = do
  createDirectoryIfMissing True (path cache </> name)
  case source of
    Git _ rev subdir -> do
      let
        cacheKey = case subdir of
          Nothing -> unCachedRev rev
          Just dir -> show $ fingerprintFingerprints [fingerprintString (unCachedRev rev), fingerprintString dir]
        addSource = AddSource name cacheKey
        src = maybe revPath (revPath </>) subdir
          where revPath = path (cachedRevPath gitCache rev)
        dst = (path $ addSourcePath cache addSource)
      alreadyInCache <- doesDirectoryExist dst
      unless alreadyInCache $ do
        checkCabalName src dep
        linkFile src dst
      return addSource
    Local dir -> do
      withTempDirectory (path cache) "tmp" $ \sandbox -> do
        let tmp = sandbox </> name
        createDirectory tmp
        cabalSdist dir tmp
        fp <- fingerprint tmp
        let addSource = AddSource name fp
        moveToAddSourceCache cache tmp dep addSource
        return addSource

gitRefToRev :: String -> Ref -> IO Rev
gitRefToRev = gitRefToRev_impl process

gitRefToRev_impl :: Fail m => Process m -> String -> Ref -> m Rev
gitRefToRev_impl Process{..} repo (Ref ref)
  | isGitRev ref = return (Rev ref)
  | otherwise = do
      r <- readProcess "git" ["ls-remote", repo, ref] ""
      case words r of
        rev : _ | isGitRev rev -> return (Rev rev)
        _ -> die ("invalid reference " ++ show ref ++ " for git repository " ++ repo)

isGitRev :: String -> Bool
isGitRev ref = length ref == 40 && all (`elem` "0123456789abcdef") ref

cabalSdist :: FilePath -> FilePath -> IO ()
cabalSdist sourceDirectory dst = do
  withCurrentDirectory sourceDirectory $ do
    callProcessM "cabal" ["sdist", "--output-directory", dst]

moveToAddSourceCache :: Path AddSourceCache -> FilePath -> AddSourceDependency a -> AddSource -> IO ()
moveToAddSourceCache cache src hpackDep dep = do
  checkCabalName src hpackDep
  let dst = addSourcePath cache dep
  unlessM (doesDirectoryExist $ path dst) $ do
    renameDirectory src $ path dst

checkCabalName :: (Fail m, MonadIO m) => FilePath -> AddSourceDependency a -> m ()
checkCabalName directory (AddSourceDependency expectedName source) = do
  name <- determinePackageName directory source
  if name == expectedName
    then return ()
    else die ("the " ++ subject source ++ " contains package " ++ show name
      ++ ", expected: " ++ show expectedName)

subject :: Source a -> String
subject addSource = case addSource of
  Git url _ subdir -> maybe "" (\dir -> "directory " ++ show dir ++ " of ") subdir ++ "git repository " ++ url
  Local dir -> "directory " ++ dir

determinePackageName :: (Fail m, MonadIO m) => FilePath -> Source a -> m String
determinePackageName directory dep = do
  cabalFile <- findCabalFile directory dep
  unPackageName . pkgName . package . packageDescription <$>
    liftIO (readPackageDescription silent cabalFile)

findCabalFile :: (Fail m, MonadIO m) => FilePath -> Source a -> m FilePath
findCabalFile dir addSource = do
  cabalFiles <- liftIO $ getCabalFiles dir
  case cabalFiles of
    [cabalFile] -> return (dir </> cabalFile)
    [] -> die ("Couldn't find .cabal file in " ++ subject addSource)
    _ -> die ("Multiple cabal files found in " ++ subject addSource)
