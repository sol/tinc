module Tinc.Setup where

import           Prelude ()
import           Prelude.Compat

import           Data.List.Compat
import           Data.Maybe
import           System.Directory
import           System.FilePath

import           Tinc.GhcInfo
import           Tinc.Sandbox
import           Tinc.Types

type Plugins = [(String, Plugin)]
type Plugin = FilePath

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
  plugins <- listPlugins pluginsDir
  return Facts {
    factsCache = cacheDir
  , factsAddSourceCache = addSourceCache
  , factsPlugins = plugins
  , factsGhcInfo = ghcInfo
  }

listPlugins :: FilePath -> IO Plugins
listPlugins pluginsDir = do
  files <- mapMaybe (stripPrefix "tinc-") <$> getDirectoryContents pluginsDir
  let f name = (name, pluginsDir </> "tinc-" ++ name)
  return (map f files)
