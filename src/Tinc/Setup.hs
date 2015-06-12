{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tinc.Setup where

import           Data.String
import           System.Directory
import           System.FilePath

data Facts = Facts {
  factsCache :: Path Cache
} deriving (Eq, Show)

data Cache

newtype Path a = Path {path :: FilePath}
  deriving (Eq, Ord, Show, IsString)

setup :: IO Facts
setup = do
  home <- getHomeDirectory
  let cache :: Path Cache
      cache = Path (home </> ".tinc" </> "cache")
  createDirectoryIfMissing True (path cache)
  return Facts {
    factsCache = cache
  }
