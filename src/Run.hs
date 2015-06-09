module Run where

import System.Directory
import System.FilePath

import Stack

run :: IO ()
run = do
  home <- getHomeDirectory
  let cache :: Path Cache
      cache = Path (home </> ".tinc" </> "cache")
  createDirectoryIfMissing True (path cache)
  installDependencies cache
