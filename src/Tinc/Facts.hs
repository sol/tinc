module Tinc.Facts where

import           Prelude ()
import           Prelude.Compat

import           Data.List.Compat
import           Control.Monad.Compat
import           Data.Maybe
import           System.Directory
import           System.Environment.Compat
import           System.FilePath
import           Data.Function

import           Tinc.GhcInfo
import           Tinc.Sandbox
import           Tinc.Types
import           Tinc.Nix (NixCache)

type Plugins = [Plugin]
type Plugin = (String, FilePath)

data Facts = Facts {
  factsCache :: Path CacheDir
, factsAddSourceCache :: Path AddSourceCache
, factsNixCache :: Path NixCache
, factsUseNix :: Bool
, factsPlugins :: Plugins
, factsGhcInfo :: GhcInfo
} deriving (Eq, Show)

useNix :: FilePath -> IO Bool
useNix executablePath = do
  maybe (isInNixStore executablePath) (`notElem` ["no", "0"]) <$> lookupEnv "TINC_USE_NIX"

isInNixStore :: FilePath -> Bool
isInNixStore = ("/nix/" `isPrefixOf`)

discoverFacts :: FilePath -> IO Facts
discoverFacts executablePath = do
  ghcInfo <- getGhcInfo
  home <- getHomeDirectory
  useNix_ <- useNix executablePath
  let pluginsDir :: FilePath
      pluginsDir = home </> ".tinc" </> "plugins"

      cacheDir :: Path CacheDir
      cacheDir = Path (home </> ".tinc" </> "cache" </> ghcFlavor ghcInfo)

      addSourceCache :: Path AddSourceCache
      addSourceCache = Path (home </> ".tinc" </> "cache" </> "add-source")

      nixCache :: Path NixCache
      nixCache = Path (home </> ".tinc" </> "cache" </> "nix")

  createDirectoryIfMissing True (path cacheDir)
  createDirectoryIfMissing True (path nixCache)
  createDirectoryIfMissing True pluginsDir
  plugins <- listAllPlugins pluginsDir
  return Facts {
    factsCache = cacheDir
  , factsAddSourceCache = addSourceCache
  , factsNixCache = nixCache
  , factsUseNix = useNix_
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
