module Run where

import System.Directory
import System.FilePath

import Stack

run :: IO ()
run = do
  home <- getHomeDirectory
  installDependencies (Path (home </> ".tinc" </> "cache") :: Path Cache)
