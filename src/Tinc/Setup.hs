module Tinc.Setup where

import           Prelude ()
import           Prelude.Compat

import           Data.List.Compat
import           Control.Monad.Compat
import           Data.Maybe
import           System.Directory
import           System.FilePath
import           Data.Function

import           Tinc.GhcInfo
import           Tinc.Sandbox
import           Tinc.Types

type Plugins = [Plugin]
type Plugin = (String, FilePath)

data Facts = Facts {
  factsCache :: Path CacheDir
, factsAddSourceCache :: Path AddSourceCache
, factsPlugins :: Plugins
, factsGhcInfo :: GhcInfo
} deriving (Eq, Show)

setup :: IO Facts
setup = do
  ghcInfo <- getGhcInfo
  home <- getHomeDirectory
  let pluginsDir :: FilePath
      pluginsDir = home </> ".tinc" </> "plugins"

      ghcFlavor :: String
      ghcFlavor = ghcInfoPlatform ghcInfo ++ "-ghc-" ++ ghcInfoVersion ghcInfo

      cacheDir :: Path CacheDir
      cacheDir = Path (home </> ".tinc" </> "cache" </> ghcFlavor)

      addSourceCache :: Path AddSourceCache
      addSourceCache = Path (home </> ".tinc" </> "cache" </> "add-source")

  createDirectoryIfMissing True (path cacheDir)
  createDirectoryIfMissing True pluginsDir
  plugins <- listAllPlugins pluginsDir
  return Facts {
    factsCache = cacheDir
  , factsAddSourceCache = addSourceCache
  , factsPlugins = plugins
  , factsGhcInfo = ghcInfo
  }

listAllPlugins :: FilePath -> IO Plugins
listAllPlugins pluginsDir = do
  plugins <- listPlugins pluginsDir
  pathPlugins <- getSearchPath >>= listPathPlugins
  return (pathPlugins ++ plugins)

listPlugins :: FilePath -> IO Plugins
listPlugins pluginsDir = do
  exists <- doesDirectoryExist pluginsDir
  if exists
    then do
      files <- mapMaybe (stripPrefix "tinc-") <$> getDirectoryContents pluginsDir
      let f name = (name, pluginsDir </> "tinc-" ++ name)
      filterM isExecutable (map f files)
    else return []

isExecutable :: Plugin -> IO Bool
isExecutable = fmap executable . getPermissions . snd

listPathPlugins :: [FilePath] -> IO Plugins
listPathPlugins = fmap (nubBy ((==) `on` fst) . concat) . mapM listPlugins
