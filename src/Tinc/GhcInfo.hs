{-# LANGUAGE CPP #-}
module Tinc.GhcInfo where

import           Prelude ()
import           Prelude.Compat

import           System.Process

import           Tinc.GhcPkg
import           Tinc.Types
import           Tinc.Fail

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
        maybe (dieLoc __FILE__ err) return (lookup name fields)
  GhcInfo
    <$> lookupField "Target platform"
    <*> lookupField "Project version"
    <*> (Path <$> lookupField "Global Package DB")
