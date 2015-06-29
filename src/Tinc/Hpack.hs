module Tinc.Hpack (
  GitDependency(..)
, extractGitDependencies
) where

import           Hpack.Config
import           System.Directory

import           Tinc.Fail

data GitDependency = GitDependency {
  gitDependencyName :: String
, gitDependencyUrl :: String
, gitDependencyRef :: String
} deriving (Eq, Show)

extractGitDependencies :: IO [GitDependency]
extractGitDependencies = do
  exists <- doesFileExist packageConfig
  if exists
    then do
      (_, package) <- readPackageConfig packageConfig >>= either die return
      return [GitDependency name url ref | Dependency name (Just (GitRef url ref)) <- packageDependencies package]
    else return []
