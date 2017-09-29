{-# LANGUAGE DeriveGeneric #-}
module Tinc.Config (
  getAdditionalDependencies
, configFile
) where

import           Data.Aeson
import           GHC.Generics
import           Hpack.Config
import           Hpack.Yaml
import           System.Directory

import           Tinc.Fail

data Config = Config {
  dependencies :: Dependencies
} deriving (Eq, Show, Generic)

instance FromJSON Config

configFile :: FilePath
configFile = "tinc.yaml"

getAdditionalDependencies :: IO Dependencies
getAdditionalDependencies = do
  exists <- doesFileExist configFile
  if exists
    then readConfig
    else return mempty

readConfig :: IO Dependencies
readConfig = decodeYaml configFile >>= either die (return . dependencies)
