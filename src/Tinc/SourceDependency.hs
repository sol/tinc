{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tinc.SourceDependency (
  GitCache
, SourceDependencyCache
, SourceDependency(..)
, SourceDependencyWithVersion
, sourceDependencyPath
, extractSourceDependencies
#ifdef TEST
, HpackSourceDependency(..)
, Source(..)
, Ref(..)
, Rev(..)
, CachedRev(..)
, mapLocalDependencyToGitDependency
, parseAddSourceDependencies
, removeDuplicates
, populateSourceDependencyCache

, copyPackageConfig

, gitClone_impl

, gitRefToRev_impl
, isGitRev
, checkCabalName
, findCabalFile
, CabalPackage(..)
, parseCabalFile
#endif
) where

import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.String
import           Data.Tree
import           Data.Version

import qualified Distribution.Version as Cabal

import           Distribution.Package hiding (Package)
import           Distribution.PackageDescription hiding (Git)
import           Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import           Distribution.Verbosity
import           GHC.Fingerprint
import qualified Hpack.Config as Hpack
import           System.Directory hiding (getDirectoryContents, withCurrentDirectory)
import           System.FilePath
import           System.IO.Temp
import           Data.Aeson
import           GHC.Generics
import           GHC.Exts

import           Tinc.Fail
import           Tinc.Types
import           Tinc.Process
import           Tinc.Hpack
import           Util

data GitCache
data SourceDependencyCache

data SourceDependency = SourceDependency {
  sourceDependencyPackageName :: String

-- This is one of:
--  + git revision for git dependencies
--  + md5(md5(git revision), md5(subdir)) for git dependencies that specify a subdir
--  + md5 of local dependency for local dependencies
, sourceDependencyHash :: String

} deriving (Eq, Show, Generic)

type SourceDependencyWithVersion = (SourceDependency, Version)

data HpackSourceDependency a = HpackSourceDependency {
  _hpackSourceDependencyName :: String
, _hpackSourceDependencySource :: Source a
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
addSourceJsonOptions = defaultOptions{fieldLabelModifier = camelTo2 '-' . drop (length ("SourceDependency" :: String))}

instance FromJSON SourceDependency where
  parseJSON = genericParseJSON addSourceJsonOptions

instance ToJSON SourceDependency where
  toJSON = genericToJSON addSourceJsonOptions

sourceDependencyPath :: Path SourceDependencyCache -> SourceDependency -> Path SourceDependency
sourceDependencyPath (Path cache) (SourceDependency name rev) = Path $ cache </> name </> rev

extractSourceDependencies :: Path GitCache -> Path SourceDependencyCache -> Hpack.Dependencies -> IO [SourceDependencyWithVersion]
extractSourceDependencies gitCache sourceDependencyCache additionalDeps = do
  parseAddSourceDependencies (toList additionalDeps) >>= fmap removeDuplicates . unfoldForestM go
  where
    go :: HpackSourceDependency Ref -> IO (SourceDependencyWithVersion, [HpackSourceDependency Ref])
    go dep@(HpackSourceDependency _ source) = do
      resolvedDep <- resolveGitReferences dep
      cachedDep <- cacheGitRev gitCache resolvedDep >>= populateSourceDependencyCache gitCache sourceDependencyCache
      deps <- addSourceDependenciesFrom sourceDependencyCache (resolvedDep, cachedDep)
      version <- cabalPackageVersion <$> parseCabalFile (path $ sourceDependencyPath sourceDependencyCache cachedDep) source
      return ((cachedDep, version), deps)

removeDuplicates :: Forest (SourceDependency, a) -> [(SourceDependency, a)]
removeDuplicates = nubByPackageName . concat . removeFakeRoot . levels . addFakeRoot
  where
    fakeRoot = error "Tinc.SourceDependency.removeDuplicates: fake-root"
    addFakeRoot = Node fakeRoot
    removeFakeRoot = drop 1
    nubByPackageName = nubBy ((==) `on` (sourceDependencyPackageName . fst))

mapLocalDependencyToGitDependency :: Source Rev -> HpackSourceDependency Ref -> HpackSourceDependency Ref
mapLocalDependencyToGitDependency source (HpackSourceDependency name dep) = case (source, dep) of
  (_, Git _ _ _) -> HpackSourceDependency name dep
  (Git url (Rev rev) subdir, Local path) -> HpackSourceDependency name (Git url (Ref rev) (Just p))
      where
        p = normalise $ fromMaybe "." subdir </> path
  (Local path, Local p) -> HpackSourceDependency name (Local $ normalise (path </> p))

resolveGitReferences :: HpackSourceDependency Ref -> IO (HpackSourceDependency Rev)
resolveGitReferences (HpackSourceDependency name source) = HpackSourceDependency name <$> case source of
  Git url ref subdir -> Git url <$> gitRefToRev url ref <*> pure subdir
  Local dir -> return (Local dir)

cacheGitRev :: Path GitCache -> HpackSourceDependency Rev -> IO (HpackSourceDependency CachedRev)
cacheGitRev cache (HpackSourceDependency name source) = HpackSourceDependency name <$> case source of
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

addSourceDependenciesFrom :: Path SourceDependencyCache -> (HpackSourceDependency Rev, SourceDependency) -> IO [HpackSourceDependency Ref]
addSourceDependenciesFrom sourceDependencyCache (HpackSourceDependency _ source, addSource) = map (mapLocalDependencyToGitDependency source) <$> do
  exists <- doesFileExist config
  if exists
    then Hpack.readPackageConfig options >>= either die (return . filterAddSource . Hpack.packageDependencies . Hpack.decodeResultPackage)
    else return []
  where
    options = Hpack.defaultDecodeOptions {Hpack.decodeOptionsTarget = config}
    config = path (sourceDependencyPath sourceDependencyCache addSource) </> Hpack.packageConfig

filterAddSource :: [(String, Hpack.DependencyVersion)] -> [HpackSourceDependency Ref]
filterAddSource deps = [HpackSourceDependency name (toSource addSource) | (name, Hpack.SourceDependency addSource) <- deps]
  where
    toSource :: Hpack.SourceDependency -> Source Ref
    toSource x = case x of
      Hpack.GitRef repo ref subdir -> Git repo (Ref ref) subdir
      Hpack.Local dir -> Local dir

parseAddSourceDependencies :: [(String, Hpack.DependencyVersion)] ->  IO [HpackSourceDependency Ref]
parseAddSourceDependencies additionalDeps = do
  exists <- doesConfigExist
  packageDeps <- if exists
    then do
      pkg <- readConfig mempty
      return $ Hpack.packageDependencies (snd pkg)
    else return []
  let deps = additionalDeps ++ packageDeps
  return (nubBy ((==) `on` _hpackSourceDependencyName) $ filterAddSource deps)

populateSourceDependencyCache :: Path GitCache -> Path SourceDependencyCache -> HpackSourceDependency CachedRev -> IO SourceDependency
populateSourceDependencyCache gitCache cache dep@(HpackSourceDependency name source) = do
  createDirectoryIfMissing True (path cache </> name)
  case source of
    Git _ rev subdir -> do
      let
        cacheKey = case subdir of
          Nothing -> unCachedRev rev
          Just dir -> show $ fingerprintFingerprints [fingerprintString (unCachedRev rev), fingerprintString dir]
        addSource = SourceDependency name cacheKey
        src = maybe revPath (revPath </>) subdir
          where revPath = path (cachedRevPath gitCache rev)
        dst = (path $ sourceDependencyPath cache addSource)
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
        copyPackageConfig dir tmp
        fp <- fingerprint tmp
        let addSource = SourceDependency name fp
        moveToSourceDependencyCache cache tmp dep addSource
        return addSource

copyPackageConfig :: FilePath -> FilePath -> IO ()
copyPackageConfig srcDir dstDir = do
  whenM (doesFileExist src) $ do
    copyFile src dst
  where
    src = srcDir </> Hpack.packageConfig
    dst = dstDir </> Hpack.packageConfig

gitRefToRev :: String -> Ref -> IO Rev
gitRefToRev = gitRefToRev_impl process {readProcess = verboseReadProcess}
  where
    verboseReadProcess command args input = do
      putStrLn (unwords $ command : args)
      r <- readProcess process command args input
      putStr r
      return r

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

moveToSourceDependencyCache :: Path SourceDependencyCache -> FilePath -> HpackSourceDependency a -> SourceDependency -> IO ()
moveToSourceDependencyCache cache src hpackDep dep = do
  checkCabalName src hpackDep
  let dst = sourceDependencyPath cache dep
  unlessM (doesDirectoryExist $ path dst) $ do
    renameDirectory src $ path dst

checkCabalName :: FilePath -> HpackSourceDependency a -> IO ()
checkCabalName directory (HpackSourceDependency expectedName source) = do
  name <- cabalPackageName <$> parseCabalFile directory source
  if name == expectedName
    then return ()
    else die ("the " ++ subject source ++ " contains package " ++ show name
      ++ ", expected: " ++ show expectedName)

subject :: Source a -> String
subject addSource = case addSource of
  Git url _ subdir -> maybe "" (\dir -> "directory " ++ show dir ++ " of ") subdir ++ "git repository " ++ url
  Local dir -> "directory " ++ dir

data CabalPackage = CabalPackage {
  cabalPackageName :: String
, cabalPackageVersion :: Version
} deriving (Eq, Show)

parseCabalFile :: FilePath -> Source a -> IO CabalPackage
parseCabalFile dir source = do
  cabalFile <- findCabalFile dir source
  pkg <- package . packageDescription <$> readGenericPackageDescription silent cabalFile
  case Cabal.versionNumbers (pkgVersion pkg) of
    [] -> die $ "the cabal file in " ++ subject source ++ " does not specify a version"
    v -> return $ CabalPackage (unPackageName $ pkgName pkg) (makeVersion v)

findCabalFile :: FilePath -> Source a -> IO FilePath
findCabalFile dir addSource = do
  cabalFiles <- getCabalFiles dir
  case cabalFiles of
    [cabalFile] -> return (dir </> cabalFile)
    [] -> die ("Couldn't find .cabal file in " ++ subject addSource)
    _ -> die ("Multiple cabal files found in " ++ subject addSource)
