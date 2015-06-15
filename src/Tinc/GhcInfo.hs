{-# LANGUAGE CPP #-}
module Tinc.GhcInfo where

import           Prelude ()
import           Prelude.Compat

import           Control.Exception
import           System.Process

import           Tinc.Types

data PackageDB

data GhcInfo = GhcInfo {
  ghcInfoGlobalPackageDB :: Path PackageDB
} deriving (Eq, Show)

getGhcInfo :: IO GhcInfo
getGhcInfo = do
  fields <- read <$> readProcess "ghc" ["--info"] ""
  globalPackageDB <- Path <$> lookupField "Global Package DB" fields
  return (GhcInfo globalPackageDB)
  where
    lookupField :: String -> [(String, String)] -> IO String
    lookupField name fields = do
      let err = "Output from `ghc --info` does not contain the field " ++ show name
      maybe (throwError err) return (lookup name fields)

throwError :: String -> IO a
throwError err = throwIO . ErrorCall $ __FILE__ ++ ": " ++ err
