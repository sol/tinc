{-# LANGUAGE DeriveGeneric #-}
module Tinc.Config (
  getAdditionalDependencies
, configFile
) where

import           GHC.Generics
import           Hpack.Config
import           Hpack.Yaml
import           System.Directory

import           Tinc.Fail

data Config = Config {
  configDependencies :: Dependencies
} deriving (Eq, Show, Generic)

instance FromValue Config

configFile :: FilePath
configFile = "tinc.yaml"

getAdditionalDependencies :: IO Dependencies
getAdditionalDependencies = do
  exists <- doesFileExist configFile
  if exists
    then readConfig
    else return mempty

readConfig :: IO Dependencies
readConfig = decodeYaml configFile >>= either die (return . configDependencies . fst) . (>>= decodeValue . snd)
