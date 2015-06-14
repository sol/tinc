module Tinc.Setup where

import           Prelude ()
import           Prelude.Compat

import           Data.List.Compat
import           Data.Maybe
import           System.Directory
import           System.FilePath

import           Tinc.Types

type Plugins = [(String, Plugin)]
type Plugin = FilePath

data Facts = Facts {
  factsCache :: Path Cache
, factsPlugins :: Plugins
} deriving (Eq, Show)

data Cache

setup :: IO Facts
setup = do
  home <- getHomeDirectory
  let cache :: Path Cache
      cache = Path (home </> ".tinc" </> "cache")
      pluginsDir = home </> ".tinc" </> "plugins"
  createDirectoryIfMissing True (path cache)
  createDirectoryIfMissing True pluginsDir
  plugins <- listPlugins pluginsDir
  return Facts {
    factsCache = cache
  , factsPlugins = plugins
  }

listPlugins :: FilePath -> IO Plugins
listPlugins pluginsDir = do
  files <- mapMaybe (stripPrefix "tinc-") <$> getDirectoryContents pluginsDir
  let f name = (name, pluginsDir </> "tinc-" ++ name)
  return (map f files)
