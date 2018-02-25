{-# LANGUAGE DeriveGeneric #-}
module Tinc.Config (
  getAdditionalDependencies
, getCompiler
, configFile
) where

import           Data.Aeson
import           Data.Maybe
import           GHC.Generics
import           Hpack.Config
import           Hpack.Yaml
import           System.Directory

import           Tinc.Fail

data Config = Config {
    compiler :: Maybe String
  , dependencies :: Maybe Dependencies
} deriving (Eq, Show, Generic)

instance FromJSON Config

configFile :: FilePath
configFile = "tinc.yaml"

getAdditionalDependencies :: IO Dependencies
getAdditionalDependencies = getConfigPart (fromMaybe mempty . dependencies)

getCompiler :: IO (Maybe String)
getCompiler = getConfigPart compiler

getConfigPart :: Monoid a => (Config -> a) -> IO a
getConfigPart configTransform = do
  exists <- doesFileExist configFile
  if exists
    then fmap configTransform readConfig
    else return mempty

readConfig :: IO Config
readConfig = decodeYaml configFile >>= either die return
