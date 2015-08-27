{-# LANGUAGE DeriveGeneric #-}
module Tinc.Config (getAdditionalDependencies) where

import           Data.Aeson
import           GHC.Generics
import           Hpack.Config
import           Hpack.Yaml
import           System.Directory

import           Tinc.Fail

data Config = Config {
  dependencies :: [Dependency]
} deriving (Eq, Show, Generic)

instance FromJSON Config

configFile :: FilePath
configFile = "tinc.yaml"

getAdditionalDependencies :: IO [Dependency]
getAdditionalDependencies = do
  exists <- doesFileExist configFile
  if exists
    then readConfig
    else return []

readConfig :: IO [Dependency]
readConfig = decodeYaml configFile >>= either die (return . dependencies)
