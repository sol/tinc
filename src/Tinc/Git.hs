{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Tinc.Git (
  clone

#ifdef TEST
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
import           Data.List
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.Verbosity
import           System.Directory
import           System.FilePath
import           System.IO.Temp

import           Tinc.Fail
import           Tinc.Hpack
import           Tinc.Process
import           Tinc.Sandbox
import           Tinc.Types
import           Util

clone :: (Fail m, Process m, MonadIO m, MonadMask m) => Path AddSourceCache -> GitDependency -> m AddSource
clone cache dep@(GitDependency name url ref) = do
  alreadyInCache <- liftIO $ doesDirectoryExist (path $ addSourcePath cache (AddSource name ref))
  AddSource name <$> if alreadyInCache then return ref else populateCache
  where
    populateCache = do
      liftIO $ createDirectoryIfMissing True (path cache)
      withTempDirectory (path cache) "tmp" $ \ sandbox -> do
        tmp <- liftIO $ createTempDirectory sandbox name

        callProcess "git" ["clone", url, tmp]
        rev <- withCurrentDirectory tmp $ do
          callProcess "git" ["reset", "--hard", ref]
          rev <- strip <$> readProcess "git" ["rev-parse", "HEAD"] ""
          liftIO $ removeDirectoryRecursive ".git"
          return rev

        checkCabalName tmp dep
        liftIO $ do
          let dst = addSourcePath cache (AddSource name rev)
          exists <- doesDirectoryExist $ path dst
          unless exists $ do
            createDirectoryIfMissing True (path cache </> name)
            renameDirectory tmp $ path dst

        return rev

checkCabalName :: (Fail m, MonadIO m) => FilePath -> GitDependency -> m ()
checkCabalName directory GitDependency{..} = do
  name <- determinePackageName directory gitDependencyUrl
  if name == gitDependencyName
    then return ()
    else die ("the git repository " ++ gitDependencyUrl ++ " contains package " ++ show name
      ++ ", expected: " ++ show gitDependencyName)

determinePackageName :: (Fail m, MonadIO m) => FilePath -> String -> m String
determinePackageName directory repo = do
  cabalFile <- findCabalFile directory repo
  unPackageName . pkgName . package . packageDescription <$>
    liftIO (readPackageDescription silent cabalFile)

findCabalFile :: (Fail m, MonadIO m) => FilePath -> String -> m FilePath
findCabalFile dir repo = do
  cabalFiles <- liftIO $ filter (".cabal" `isSuffixOf`) <$> getDirectoryContents dir
  case cabalFiles of
    [cabalFile] -> return (dir </> cabalFile)
    [] -> die ("Couldn't find .cabal file in git repository: " ++ repo)
    _ -> die ("Multiple cabal files found in git repository: " ++ repo)
