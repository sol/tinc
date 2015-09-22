{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Helper (
  module Test.Hspec

, ensureCache
, cacheDir

, setenv
, setenvSandbox

, hspecDiscover
, hspecDiscoverSandbox

, genericsSop
, getoptGenerics
, getoptGenericsPackages
, getoptGenericsSandbox

, removeDirectory
) where

import           Prelude ()
import           Prelude.Compat

import           Test.Hspec

import           Control.Monad
import qualified Data.Graph.Wrapper as G
import           Data.List
import           Data.String
import           Shelly (shelly, rm_rf, cp_r)
import           System.Directory hiding (removeDirectory)
import           System.FilePath
import           System.Process
import           Test.Mockery.Directory

import           Tinc.Package
import           Tinc.Sandbox
import           Tinc.Types
import           Util

instance (Ord a, Ord v) => Eq (G.Graph a v) where
  a == b = sort (G.toList a) == sort (G.toList b)

instance IsString Version where
  fromString version = Version version Nothing

ensureCache :: IO ()
ensureCache = do
  mkCache
  restoreCache

withDirectory :: FilePath -> IO a -> IO a
withDirectory dir action = do
  createDirectoryIfMissing True dir
  withCurrentDirectory dir action

setenv :: Package
setenv = Package "setenv" "0.1.1.3"

getoptGenerics :: Package
getoptGenerics = Package "getopt-generics" "0.6.3"

genericsSop :: Package
genericsSop = Package "generics-sop" "0.1.1.2"

hspecDiscover :: Package
hspecDiscover = Package "hspec-discover" "2.1.7"

getoptGenericsPackages :: [Package]
getoptGenericsPackages = [
    Package "base-compat" "0.8.2"
  , Package "base-orphans" "0.3.2"
  , genericsSop
  , Package "tagged" "0.8.0.1"
  , getoptGenerics
  ]

mkTestSandbox :: String -> [Package] -> IO ()
mkTestSandbox pattern packages = do
  let sandbox = toSandbox pattern
  exists <- doesDirectoryExist $ path sandbox
  when (not exists) $ do
    createDirectoryIfMissing True $ path sandbox
    withDirectory (path sandbox) $ do
      callCommand "cabal sandbox init"
      callCommand ("cabal install --disable-library-profiling --disable-optimization --disable-documentation " ++
                   unwords (map showPackage packages))
      touch "tinc.valid.v2"

mkCache :: IO ()
mkCache = do
  exists <- doesDirectoryExist (path cacheBackup)
  unless exists $ do
    removeDirectory cacheDir
    createDirectory (path cacheDir)
    mkTestSandbox "setenv" [setenv]
    mkTestSandbox "getopt-generics" getoptGenericsPackages
    mkTestSandbox "hspec-discover" [hspecDiscover]
    copyDirectory cacheDir cacheBackup

restoreCache :: IO ()
restoreCache = do
  removeDirectory cacheDir
  copyDirectory cacheBackup cacheDir
  forM_ [setenvSandbox, getoptGenericsSandbox, hspecDiscoverSandbox] $ \ sandbox -> do
    Path packageDb <- findPackageDb sandbox
    touch (packageDb </> "package.cache")

cacheDir :: Path CacheDir
cacheDir = "/tmp/tinc-test-cache"

cacheBackup :: Path CacheDir
cacheBackup = "cache-backup"

copyDirectory :: Path a -> Path a -> IO ()
copyDirectory src dst = shelly $ do
  cp_r (fromString $ path src) (fromString $ path dst)

removeDirectory :: Path a -> IO ()
removeDirectory dir = shelly $ do
  rm_rf (fromString $ path dir)

toSandbox :: String -> Path Sandbox
toSandbox pattern = Path (path cacheDir </> "tinc-" ++ pattern)

setenvSandbox :: Path Sandbox
setenvSandbox = toSandbox "setenv"

getoptGenericsSandbox :: Path Sandbox
getoptGenericsSandbox = toSandbox "getopt-generics"

hspecDiscoverSandbox :: Path Sandbox
hspecDiscoverSandbox = toSandbox "hspec-discover"
