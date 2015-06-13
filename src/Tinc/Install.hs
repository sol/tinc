{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinc.Install (
  Path (..)
, Sandbox
, PackageDB
, PackageConfig
, Cache
, installDependencies

-- exported for testing
, findPackageDB
, extractPackages
, isPackageDB
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Exception
import           Control.Monad.Compat
import           Data.Function
import           Data.Graph.Wrapper
import           Data.List.Compat
import           Data.Maybe
import           System.Directory
import           System.Exit.Compat
import           System.FilePath
import           System.IO.Temp
import           System.Process

import           Tinc.Setup
import           Package
import           PackageGraph
import           Util

data Sandbox
data PackageDB
data PackageConfig

currentDirectory :: Path Sandbox
currentDirectory = "."

initSandbox :: IO (Path PackageDB)
initSandbox = do
  callCommand "cabal sandbox init"
  findPackageDB currentDirectory

deleteSandbox :: IO ()
deleteSandbox = callCommand "cabal sandbox delete"

installDependencies :: Path Cache -> IO ()
installDependencies cache = do
  exists <- doesDirectoryExist ".cabal-sandbox"
  when exists deleteSandbox
  installPlan <- parseInstallPlan <$> readProcess "cabal" command ""
  (packages, packageConfigs)  <- unzip <$> findReusablePackages cache installPlan
  if null (installPlan \\ packages)
    then do
      destPackageDB <- initSandbox
      registerPackageConfigs destPackageDB packageConfigs
    else do
      createCacheSandbox cache installPlan packageConfigs
  where
    command :: [String]
    command = words "install --only-dependencies --enable-tests --dry-run --package-db=clear --package-db=global"

createCacheSandbox :: Path Cache -> [Package] -> [Path PackageConfig] -> IO ()
createCacheSandbox cache installPlan packageConfigs = do
  basename <- takeBaseName <$> getCurrentDirectory
  sandbox <- createTempDirectory (path cache) (basename ++ "-")
  create sandbox packageConfigs `onException` removeDirectoryRecursive sandbox
  cloneSandbox (Path sandbox)
  where
    create sandbox cachedPackages = do
      withCurrentDirectory sandbox $ do
        destPackageDB <- initSandbox
        registerPackageConfigs destPackageDB cachedPackages
        callProcess "cabal" ("install" : map showPackage installPlan)

findReusablePackages :: Path Cache -> [Package] -> IO [(Package, Path PackageConfig)]
findReusablePackages cache installPlan = do
  sandboxes <- lookupSandboxes cache
  globalPackages <- listGlobalPackages
  cachedPackages <- forM sandboxes $ \ sandbox -> do
    packageDB <- findPackageDB sandbox
    cacheGraph <- readPackageGraph packageDB
    let packages = nubBy ((==) `on` packageName) (installPlan ++ globalPackages)
        reusable = reusablePackages packages cacheGraph
    lookupPackages packageDB reusable
  return $ ordNub $ concat cachedPackages

lookupSandboxes :: Path Cache -> IO [Path Sandbox]
lookupSandboxes (Path cache) = map Path <$> listDirectories cache

listGlobalPackages :: IO [Package]
listGlobalPackages = do
  packageDB <- findGlobalPackageDB
  vertices <$> readPackageGraph_ [packageDB]

cloneSandbox :: Path Sandbox -> IO ()
cloneSandbox source = do
  sourcePackageDB <- findPackageDB source
  destPackageDB <- initSandbox
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
  map snd <$> lookupPackages packageDB (reverse $ topologicalSort graph)

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

lookupPackages :: Path PackageDB -> [Package] -> IO [(Package, Path PackageConfig)]
lookupPackages packageDB packages = do
  packageConfigs <- findPackageConfigs packageDB
  fmap catMaybes . forM packages $ \ package ->
    case lookupPackage package packageConfigs of
      Right (Just packageConfig) -> return $ Just (package, Path $ path packageDB </> packageConfig)
      Right Nothing -> return Nothing
      Left err -> die err

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
