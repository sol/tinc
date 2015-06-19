{-# LANGUAGE CPP #-}
module Tinc.GhcInfo where

import           Prelude ()
import           Prelude.Compat

import           Control.Exception
import           System.Process

import           Tinc.Types

data PackageDb

data GhcInfo = GhcInfo {
  ghcInfoPlatform :: String
, ghcInfoVersion :: String
, ghcInfoGlobalPackageDb :: Path PackageDb
} deriving (Eq, Show)

getGhcInfo :: IO GhcInfo
getGhcInfo = do
  fields <- read <$> readProcess "ghc" ["--info"] ""
  let lookupField :: String -> IO String
      lookupField name = do
        let err = "Output from `ghc --info` does not contain the field " ++ show name
        maybe (throwError err) return (lookup name fields)
  GhcInfo
    <$> lookupField "Target platform"
    <*> lookupField "Project version"
    <*> (Path <$> lookupField "Global Package DB")

throwError :: String -> IO a
throwError err = throwIO . ErrorCall $ __FILE__ ++ ": " ++ err
