module Tinc.Hpack (
  GitDependency(..)
, extractGitDependencies
) where

import           Hpack.Config

import           Tinc.Fail
import           Tinc.Git

extractGitDependencies :: IO [GitDependency]
extractGitDependencies = do
  (_, package) <- readPackageConfig packageConfig >>= either die return
  return [GitDependency name url ref | Dependency name (Just (GitRef url ref)) <- packageDependencies package]
