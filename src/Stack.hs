{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Stack (
  Path (..)
, Sandbox
, PackageDB
, PackageConfig
, Cache
, installDependencies
, createStackedSandbox

-- exported for testing
, findPackageDB
, extractPackages
, isPackageDB
) where

import           Prelude ()
import           Prelude.Compat

import           Data.Foldable
import           Data.Function
import           Data.Graph.Wrapper
import           Data.List.Compat
import           Data.Maybe
import           Data.String
import           Data.Traversable
import           System.Directory
import           System.Exit.Compat
import           System.FilePath
import           System.IO.Temp
import           System.Process

import           Package
import           PackageGraph
import           Util

newtype Path a = Path {path :: FilePath}
  deriving (Eq, Ord, Show, IsString)

data Sandbox
data PackageDB
data PackageConfig
data Cache

currentDirectory :: Path Sandbox
currentDirectory = "."

initSandbox :: IO ()
initSandbox = callCommand "cabal sandbox init"

installDependencies :: Path Cache -> IO ()
installDependencies cache = do
  initSandbox
  installPlan <- parseInstallPlan <$> readProcess "cabal" (command ++ ["--dry-run"]) ""
  sandbox <- createCacheSandbox cache installPlan
  cloneSandbox sandbox
  where
    command :: [String]
    command = words "install --only-dependencies"

createCacheSandbox :: Path Cache -> [Package] -> IO (Path Sandbox)
createCacheSandbox cache installPlan = do
  cachedPackages <- lookup_ cache installPlan
  sandbox <- createTempDirectory (path cache) "sandbox"
  withCurrentDirectory sandbox $ do
    initSandbox
    destPackageDB <- findPackageDB currentDirectory
    registerPackageConfigs destPackageDB cachedPackages
    callProcess "cabal" ("install" : map showPackage installPlan)
  return (Path sandbox)

lookup_ :: Path Cache -> [Package] -> IO [Path PackageConfig]
lookup_ cache installPlan = do
  sandboxes <- lookupSandboxes cache
  globalPackages <- listGlobalPackages
  cachedPackages <- forM sandboxes $ \ sandbox -> do
    packageDB <- findPackageDB sandbox
    cacheGraph <- readPackageGraph packageDB
    let reusable = findReusablePackages installPlan globalPackages cacheGraph
    lookupPackages packageDB reusable
  return $ ordNub $ concat cachedPackages

lookupSandboxes :: Path Cache -> IO [Path Sandbox]
lookupSandboxes (Path cache) = map Path <$> listDirectories cache

findReusablePackages :: [Package] -> [Package] -> PackageGraph -> [Package]
findReusablePackages installPlan globalPackages cache =
  reusablePackages packages cache
  where
    packages = nubBy ((==) `on` packageName) (installPlan ++ globalPackages)

listGlobalPackages :: IO [Package]
listGlobalPackages = do
  packageDB <- findGlobalPackageDB
  vertices <$> readPackageGraph_ [packageDB]

createStackedSandbox :: Path Sandbox -> IO ()
createStackedSandbox source = do
  initSandbox
  cloneSandbox source

cloneSandbox :: Path Sandbox -> IO ()
cloneSandbox source = do
  sourcePackageDB <- findPackageDB source
  destPackageDB <- findPackageDB currentDirectory
  packages <- extractPackages sourcePackageDB
  registerPackageConfigs destPackageDB packages

findPackageDB :: Path Sandbox -> IO (Path PackageDB)
findPackageDB sandbox = do
  xs <- getDirectoryContents sandboxDir
  case listToMaybe (filter isPackageDB xs) of
    Just p -> Path <$> canonicalizePath (sandboxDir </> p)
    Nothing -> die ("package db not found in " ++ sandboxDir)
  where
    sandboxDir = path sandbox </> ".cabal-sandbox"

isPackageDB :: FilePath -> Bool
isPackageDB = ("-packages.conf.d" `isSuffixOf`)

extractPackages :: Path PackageDB -> IO [Path PackageConfig]
extractPackages packageDB = do
  graph <- readPackageGraph packageDB
  lookupPackages packageDB $ reverse $ topologicalSort graph

readPackageGraph :: Path PackageDB -> IO PackageGraph
readPackageGraph packageDB = do
  globalPackageDB <- findGlobalPackageDB
  readPackageGraph_ [globalPackageDB, packageDB]

readPackageGraph_ :: [Path PackageDB] -> IO PackageGraph
readPackageGraph_ packageDBs = do
  dot <- readProcess "ghc-pkg"
    ((concatMap (\ pdb -> ["--package-db", path pdb]) packageDBs) ++
     "dot" : []) []
  case fromDot dot of
    Right graph -> return graph
    Left message -> die message

findPackageConfigs :: Path PackageDB -> IO [FilePath]
findPackageConfigs packageDB =
  filter (".conf" `isSuffixOf`) <$> getDirectoryContents (path packageDB)

lookupPackages :: Path PackageDB -> [Package] -> IO [Path PackageConfig]
lookupPackages packageDB packages = do
  packageConfigs <- findPackageConfigs packageDB
  fmap catMaybes . forM packages $ \ package ->
    case lookupPackage package packageConfigs of
      Right x -> return (Path . (path packageDB </>) <$> x)
      Left message -> die message

findGlobalPackageDB :: IO (Path PackageDB)
findGlobalPackageDB = do
  output <- readProcess "ghc-pkg" ["list"] []
  return $ Path $ takeWhile (/= ':') output

registerPackageConfigs :: Path PackageDB -> [Path PackageConfig] -> IO ()
registerPackageConfigs packageDB packages = do
  forM_ packages $ \ package ->
    copyFile (path package) (path packageDB </> takeFileName (path package))
  callProcess "cabal" $
    "exec" :
    "--" :
    "ghc-pkg" :
    "recache" :
    "--package-db" : path packageDB :
    []
