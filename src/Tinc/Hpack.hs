{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
module Tinc.Hpack (
  readConfig
, doesConfigExist
, render
, mkPackage
, extractAddSourceDependencies

#ifdef TEST
, parseAddSourceDependencies
, cacheAddSourceDep
, checkCabalName
, determinePackageName
, findCabalFile
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Catch
import           Control.Monad.Compat
import           Control.Monad.IO.Class
import           Data.Function
import           Data.List.Compat
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.Verbosity
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
readConfig deps = Hpack.readPackageConfig Hpack.packageConfig >>= either die (return . addDependencies . snd)
  where
    addDependencies :: Hpack.Package -> Hpack.Package
    addDependencies p
      | null deps = p
      | otherwise = p { Hpack.packageName = "tinc-generated", Hpack.packageExecutables = mkExecutable deps : Hpack.packageExecutables p }

doesConfigExist :: IO Bool
doesConfigExist = doesFileExist Hpack.packageConfig

render :: Hpack.Package -> (FilePath, String)
render pkg = (name, contents)
  where
    name = Hpack.packageName pkg ++ ".cabal"
    contents = renderPackage defaultRenderSettings 2 [] pkg

mkPackage :: [Hpack.Dependency] -> Hpack.Package
mkPackage deps = (Hpack.package "tinc-generated" "0.0.0"){Hpack.packageExecutables = [mkExecutable deps]}

mkExecutable :: [Hpack.Dependency] -> Hpack.Section Hpack.Executable
mkExecutable deps = (Hpack.section $ Hpack.Executable "tinc-generated" "Generated.hs" []){Hpack.sectionDependencies = deps}

extractAddSourceDependencies :: Path AddSourceCache -> [Hpack.Dependency] -> IO [Sandbox.AddSource]
extractAddSourceDependencies addSourceCache addSourceDeps =
  parseAddSourceDependencies addSourceDeps >>= mapM (uncurry (cacheAddSourceDep addSourceCache))

parseAddSourceDependencies :: [Hpack.Dependency] ->  IO [(String, Hpack.AddSource)]
parseAddSourceDependencies additionalDeps = do
  exists <- doesFileExist Hpack.packageConfig
  packageDeps <- if exists
    then do
      pkg <- readConfig []
      return $ Hpack.packageDependencies pkg
    else return []
  let deps = nubBy ((==) `on` Hpack.dependencyName) (additionalDeps ++ packageDeps)
  return [(name, addSource) | Hpack.Dependency name (Just addSource) <- deps]

cacheAddSourceDep :: (Fail m, Process m, MonadIO m, MonadMask m) => Path AddSourceCache -> String -> Hpack.AddSource -> m Sandbox.AddSource
cacheAddSourceDep cache name dep = do
  liftIO $ createDirectoryIfMissing True (path cache)
  withTempDirectory (path cache) "tmp" $ \ sandbox -> do
    let tmp = sandbox </> name
    liftIO $ createDirectory tmp
    case dep of
      Hpack.GitRef url hashCandidate -> do
        let addSourceCandidate = AddSource name hashCandidate
        alreadyInCache <- liftIO $ doesDirectoryExist (path $ addSourcePath cache addSourceCandidate)
        if alreadyInCache
          then return addSourceCandidate
          else do
            rev <- cloneGit url hashCandidate tmp
            let addSource = AddSource name rev
            moveToAddSourceCache cache tmp dep addSource
            return addSource
      Hpack.Local dir -> liftIO $ do
        cabalSdist dir tmp
        fp <- fingerprint tmp
        let addSource = AddSource name fp
        moveToAddSourceCache cache tmp dep addSource
        return addSource

cloneGit :: (Process m, MonadIO m, MonadMask m) => String -> String -> FilePath -> m String
cloneGit url ref dst = do
  callProcess "git" ["clone", url, dst]
  withCurrentDirectory dst $ do
    callProcess "git" ["reset", "--hard", ref]
    rev <- strip <$> readProcess "git" ["rev-parse", "HEAD"] ""
    liftIO $ removeDirectoryRecursive ".git"
    return rev

cabalSdist :: FilePath -> FilePath -> IO ()
cabalSdist sourceDirectory dst = do
  withCurrentDirectory sourceDirectory $ do
    callProcess "cabal" ["sdist", "--output-directory", dst]

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
  Hpack.GitRef url _ -> "git repository " ++ url
  Hpack.Local dir -> "directory " ++ dir

determinePackageName :: (Fail m, MonadIO m) => FilePath -> Hpack.AddSource -> m String
determinePackageName directory dep = do
  cabalFile <- findCabalFile directory dep
  unPackageName . pkgName . package . packageDescription <$>
    liftIO (readPackageDescription silent cabalFile)

findCabalFile :: (Fail m, MonadIO m) => FilePath -> Hpack.AddSource -> m FilePath
findCabalFile dir addSource = do
  cabalFiles <- liftIO $ filter (".cabal" `isSuffixOf`) <$> getDirectoryContents dir
  case cabalFiles of
    [cabalFile] -> return (dir </> cabalFile)
    [] -> die ("Couldn't find .cabal file in " ++ subject addSource)
    _ -> die ("Multiple cabal files found in " ++ subject addSource)
