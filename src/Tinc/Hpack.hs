module Tinc.Hpack (
  GitDependency(..)
, extractGitDependencies
, readConfig
, doesConfigExist
, render
, mkPackage
) where

import           Data.Function
import           Data.List
import           Hpack.Config
import           Hpack.Run
import           System.Directory

import           Tinc.Config
import           Tinc.Fail

data GitDependency = GitDependency {
  gitDependencyName :: String
, gitDependencyUrl :: String
, gitDependencyRef :: String
} deriving (Eq, Show)

extractGitDependencies :: IO [GitDependency]
extractGitDependencies = do
  additionalDeps <- getAdditionalDependencies
  exists <- doesFileExist packageConfig
  packageDeps <- if exists
    then do
      pkg <- readConfig []
      return $ packageDependencies pkg
    else return []
  let deps = nubBy ((==) `on` dependencyName) (additionalDeps ++ packageDeps)
  return [GitDependency name url ref | Dependency name (Just (GitRef url ref)) <- deps]

readConfig :: [Dependency] -> IO Package
readConfig deps = readPackageConfig packageConfig >>= either die (return . addDependencies . snd)
  where
    addDependencies :: Package -> Package
    addDependencies p
      | null deps = p
      | otherwise = p { packageName = "tinc-generated", packageExecutables = mkExecutable deps : packageExecutables p }

doesConfigExist :: IO Bool
doesConfigExist = doesFileExist packageConfig

render :: Package -> (FilePath, String)
render pkg = (name, contents)
  where
    name = packageName pkg ++ ".cabal"
    contents = renderPackage defaultRenderSettings 0 [] pkg

mkPackage :: [Dependency] -> Package
mkPackage deps = (package "tinc-generated" "0.0.0"){packageExecutables = [mkExecutable deps]}

mkExecutable :: [Dependency] -> Section Executable
mkExecutable deps = (section $ Executable "tinc-generated" "Generated.hs" []){sectionDependencies = deps}
