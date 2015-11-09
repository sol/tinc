{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Tinc.Freeze (
  writeFreezeFile
, readFreezeFile
) where

import           Data.Aeson
import qualified Data.ByteString as B
import           Data.Char
import           Data.Ord
import           Data.List
import qualified Data.Yaml as Yaml
import           Hpack.Yaml
import           GHC.Generics
import           System.Directory

import           Tinc.Fail
import           Tinc.Package

type Constraint = String

freezeFile :: FilePath
freezeFile = "tinc.freeze"

data Dependency = Dependency {
  name :: String
, version :: String
} deriving (Eq, Show, Generic)

instance FromJSON Dependency

instance ToJSON Dependency where
  toJSON = genericToJSON defaultOptions

data FreezeFile = FreezeFile {
  dependencies :: [Dependency]
} deriving (Eq, Show, Generic)

instance FromJSON FreezeFile

instance ToJSON FreezeFile where
  toJSON = genericToJSON defaultOptions

writeFreezeFile :: [Package] -> IO ()
writeFreezeFile deps = B.writeFile freezeFile (Yaml.encode contents)
  where
    contents = FreezeFile (sortByName $ map toDependency deps)

sortByName :: [Dependency] -> [Dependency]
sortByName = sortBy $ comparing f
  where
    f :: Dependency -> (String, String)
    f (Dependency n _) = (map toLower n, n)

toDependency :: Package -> Dependency
toDependency (Package n (Version v _)) = Dependency {name =  n, version = v}

toConstraint :: Dependency -> Constraint
toConstraint (Dependency n v) = "--constraint=" ++ n ++ " == " ++ v

readFreezeFile :: IO [Constraint]
readFreezeFile = do
  exists <- doesFileExist freezeFile
  if exists
    then decodeYaml freezeFile >>= either die (return . map toConstraint . dependencies)
    else return []
