{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Tinc.Hpack (
  readConfig
, doesConfigExist
, render
, mkPackage
, extractAddSourceDependencies

#ifdef TEST
, extractAddSourceDependencies_impl
, parseAddSourceDependencies
, populateAddSourceCache_impl

, CloneGit
, cloneGit_impl

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
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.Verbosity
import           GHC.Fingerprint
import qualified Hpack.Config as Hpack
import           Hpack.Run
import           System.Directory hiding (getDirectoryContents)
import           System.FilePath
import           System.IO.Temp

import           Tinc.Fail
import           Tinc.Process
import           Tinc.Sandbox as Sandbox
import           Tinc.Types
import           Util

readConfig :: [Hpack.Dependency] -> IO Hpack.Package
readConfig additionalDeps = Hpack.readPackageConfig Hpack.packageConfig >>= either die (return . addDependencies . snd)
  where
    addDependencies :: Hpack.Package -> Hpack.Package
    addDependencies p
      | null additionalDeps = p
      | otherwise = (Hpack.renamePackage "tinc-generated" p) {Hpack.packageExecutables = mkExecutable additionalDeps : Hpack.packageExecutables p}

doesConfigExist :: IO Bool
doesConfigExist = doesFileExist Hpack.packageConfig

render :: Hpack.Package -> (FilePath, String)
render pkg = (name, contents)
  where
    name :: String
    name = Hpack.packageName pkg ++ ".cabal"

    contents :: String
    contents = renderPackage defaultRenderSettings 2 [] [] pkg

mkPackage :: [Hpack.Dependency] -> Hpack.Package
mkPackage deps = (Hpack.package "tinc-generated" "0.0.0"){Hpack.packageExecutables = [mkExecutable deps]}

mkExecutable :: [Hpack.Dependency] -> Hpack.Section Hpack.Executable
mkExecutable deps = (Hpack.section $ Hpack.Executable "tinc-generated" "Generated.hs" []){Hpack.sectionDependencies = deps}

extractAddSourceDependencies :: Path AddSourceCache -> [Hpack.Dependency] -> IO [Sandbox.AddSource]
extractAddSourceDependencies addSourceCache additionalDeps =
  parseAddSourceDependencies additionalDeps >>= extractAddSourceDependencies_impl (addSourceDependenciesFrom addSourceCache) resolveGitReferences (populateAddSourceCache addSourceCache)

extractAddSourceDependencies_impl :: AddSourceDependenciesFrom -> ResolveGitReferences -> PopulateAddSourceCache -> [(String, Hpack.AddSource)] -> IO [Sandbox.AddSource]
extractAddSourceDependencies_impl addSourceDependenciesFrom_ resolveGitReferences_ populateAddSourceCache_ = go
  where
    go deps = do
      case deps of
        [] -> return []
        _ -> do
          xs <- mapM resolveGitReferences_ deps >>= mapM populateAddSourceCache_
          (xs ++) <$> (mapM addSourceDependenciesFrom_ xs >>= go . concat)

type ResolveGitReferences = (String, Hpack.AddSource) -> IO (String, Hpack.AddSource)

resolveGitReferences :: ResolveGitReferences
resolveGitReferences (name, addSource) = (,) name <$> case addSource of
  Hpack.GitRef url ref dir -> Hpack.GitRef url <$> gitRefToRev url ref <*> pure dir
  Hpack.Local _ -> return addSource

type AddSourceDependenciesFrom = AddSource -> IO [(String, Hpack.AddSource)]

addSourceDependenciesFrom :: Path AddSourceCache -> AddSourceDependenciesFrom
addSourceDependenciesFrom addSourceCache addSource = do
  exists <- doesFileExist config
  if exists
    then Hpack.readPackageConfig config >>= either die (return . filterAddSource . Hpack.packageDependencies . snd)
    else return []
  where
    config = path (addSourcePath addSourceCache addSource) </> Hpack.packageConfig

filterAddSource :: [Hpack.Dependency] -> [(String, Hpack.AddSource)]
filterAddSource deps = [(name, addSource) | Hpack.Dependency name (Just addSource) <- deps]

parseAddSourceDependencies :: [Hpack.Dependency] ->  IO [(String, Hpack.AddSource)]
parseAddSourceDependencies additionalDeps = do
  exists <- doesConfigExist
  packageDeps <- if exists
    then do
      pkg <- readConfig []
      return $ Hpack.packageDependencies pkg
    else return []
  let deps = nubBy ((==) `on` Hpack.dependencyName) (additionalDeps ++ packageDeps)
  return (filterAddSource deps)

type PopulateAddSourceCache = (String, Hpack.AddSource) -> IO Sandbox.AddSource

populateAddSourceCache :: Path AddSourceCache -> PopulateAddSourceCache
populateAddSourceCache = populateAddSourceCache_impl (cloneGit_impl process)

populateAddSourceCache_impl :: CloneGit IO -> Path AddSourceCache -> PopulateAddSourceCache
populateAddSourceCache_impl cloneGit cache (name, dep) = do
  createDirectoryIfMissing True (path cache)
  withTempDirectory (path cache) "tmp" $ \ sandbox -> do
    let tmp = sandbox </> name
    createDirectory tmp
    case dep of
      Hpack.GitRef url rev subdir -> do
        let
          cacheKey = case subdir of
            Nothing -> rev
            Just dir -> show $ fingerprintFingerprints [fingerprintString rev, fingerprintString dir]
          src = maybe tmp (tmp </>) subdir
          addSource = AddSource name cacheKey
        alreadyInCache <- doesDirectoryExist (path $ addSourcePath cache addSource)
        unless alreadyInCache $ do
          cloneGit url rev tmp
          moveToAddSourceCache cache src dep addSource
        return addSource
      Hpack.Local dir -> do
        cabalSdist dir tmp
        fp <- fingerprint tmp
        let addSource = AddSource name fp
        moveToAddSourceCache cache tmp dep addSource
        return addSource

gitRefToRev :: String -> String -> IO String
gitRefToRev = gitRefToRev_impl process

gitRefToRev_impl :: Fail m => Process m -> String -> String -> m String
gitRefToRev_impl Process{..} repo ref
  | isGitRev ref = return ref
  | otherwise = do
      r <- readProcess "git" ["ls-remote", repo, ref] ""
      case words r of
        rev : _ | isGitRev rev -> return rev
        _ -> die ("invalid reference " ++ show ref ++ " for git repository " ++ repo)

isGitRev :: String -> Bool
isGitRev ref = length ref == 40 && all (`elem` "0123456789abcdef") ref

type CloneGit m = String -> String -> FilePath -> m ()

cloneGit_impl :: Process IO -> String -> String -> FilePath -> IO ()
cloneGit_impl Process{..} url rev dst = do
  callProcess "git" ["clone", url, dst]
  withCurrentDirectory dst $ do
    callProcess "git" ["reset", "--hard", rev]
    removeDirectoryRecursive ".git"

cabalSdist :: FilePath -> FilePath -> IO ()
cabalSdist sourceDirectory dst = do
  withCurrentDirectory sourceDirectory $ do
    callProcessM "cabal" ["sdist", "--output-directory", dst]

moveToAddSourceCache :: MonadIO m => Path AddSourceCache -> FilePath -> Hpack.AddSource -> Sandbox.AddSource -> m ()
moveToAddSourceCache cache src hpackDep dep@(AddSource name _) = liftIO $ do
  checkCabalName src name hpackDep
  let dst = addSourcePath cache dep
  exists <- doesDirectoryExist $ path dst
  unless exists $ do
    createDirectoryIfMissing True (path cache </> name)
    renameDirectory src $ path dst

checkCabalName :: (Fail m, MonadIO m) => FilePath -> String -> Hpack.AddSource -> m ()
checkCabalName directory expectedName addSource = do
  name <- determinePackageName directory addSource
  if name == expectedName
    then return ()
    else die ("the " ++ subject addSource ++ " contains package " ++ show name
      ++ ", expected: " ++ show expectedName)

subject :: Hpack.AddSource -> String
subject addSource = case addSource of
  Hpack.GitRef url _ subdir -> maybe "" (\dir -> "directory " ++ show dir ++ " of ") subdir ++ "git repository " ++ url
  Hpack.Local dir -> "directory " ++ dir

determinePackageName :: (Fail m, MonadIO m) => FilePath -> Hpack.AddSource -> m String
determinePackageName directory dep = do
  cabalFile <- findCabalFile directory dep
  unPackageName . pkgName . package . packageDescription <$>
    liftIO (readPackageDescription silent cabalFile)

findCabalFile :: (Fail m, MonadIO m) => FilePath -> Hpack.AddSource -> m FilePath
findCabalFile dir addSource = do
  cabalFiles <- liftIO $ getCabalFiles dir
  case cabalFiles of
    [cabalFile] -> return (dir </> cabalFile)
    [] -> die ("Couldn't find .cabal file in " ++ subject addSource)
    _ -> die ("Multiple cabal files found in " ++ subject addSource)
