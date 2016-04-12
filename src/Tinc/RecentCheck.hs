{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Tinc.RecentCheck (
  isRecent
, markRecent
, tincEnvCreationTime
) where

import           Data.Maybe
import           Data.Time
import           System.Directory
import           System.FilePath

import           Tinc.Facts
import           Tinc.Nix
import           Tinc.Sandbox
import           Tinc.GhcInfo
import qualified Tinc.Config as Tinc
import qualified Hpack.Config as Hpack
import           Tinc.Freeze (freezeFile)
import           Util

isRecent :: Maybe UTCTime -> IO Bool
isRecent envCreationTime = case envCreationTime of
  Just packageMTime -> modificationTime freezeFile >>= \case
    Just freezeMTime -> do
      cabalFiles <- getCabalFiles "."
      xs <- mapM modificationTime (Tinc.configFile : Hpack.packageConfig : cabalFiles)
      return $ maximum (freezeMTime : catMaybes xs) < packageMTime
    Nothing -> return False
  Nothing -> return False

tincEnvCreationTime :: Facts -> IO (Maybe UTCTime)
tincEnvCreationTime Facts{..} = if factsUseNix
  then packageDotNixCreationTime
  else sandboxCreationTime factsGhcInfo

modificationTime :: FilePath -> IO (Maybe UTCTime)
modificationTime file = do
  exists <- doesFileExist file
  if exists then Just <$> getModificationTime file else return Nothing

packageDotNixCreationTime :: IO (Maybe UTCTime)
packageDotNixCreationTime = modificationTime resolverFile

recentMarker :: GhcInfo -> FilePath
recentMarker ghcInfo = cabalSandboxDirectory </> ghcFlavor ghcInfo ++ ".tinc"

sandboxCreationTime ::  GhcInfo -> IO (Maybe UTCTime)
sandboxCreationTime ghcInfo = do
  modificationTime $ recentMarker ghcInfo

markRecent :: GhcInfo -> IO ()
markRecent ghcInfo = do
  writeFile (recentMarker ghcInfo) ""
