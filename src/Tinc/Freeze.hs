{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Tinc.Freeze (
  writeFreezeFile
, readFreezeFile
, freezeFile
) where

import           Data.Aeson
import qualified Data.ByteString as B
import           Data.Char
import           Data.Ord
import           Data.List
import qualified Data.Yaml as Yaml
import           Hpack.Yaml
import           GHC.Generics
import           Control.Exception
import           Control.Monad
import           System.IO.Error
import           System.Directory

import           Tinc.Fail
import           Tinc.Package
import           Tinc.Sandbox

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
writeFreezeFile deps = do
  old <- either (const Nothing) Just <$> tryJust (guard . isDoesNotExistError) (B.readFile freezeFile)
  unless (Just contents == old) $ do
    B.writeFile freezeFile contents
  where
    contents = Yaml.encode $ FreezeFile (sortByName $ map toDependency deps)

sortByName :: [Dependency] -> [Dependency]
sortByName = sortBy $ comparing f
  where
    f :: Dependency -> (String, String)
    f (Dependency n _) = (map toLower n, n)

toDependency :: Package -> Dependency
toDependency (Package n (Version v _)) = Dependency {name =  n, version = v}

toConstraint :: Dependency -> Constraint
toConstraint (Dependency n v) = "--constraint=" ++ n ++ " == " ++ v

readFreezeFile :: [AddSource] -> IO [Constraint]
readFreezeFile (map addSourcePackageName -> addSourceDependencies) = do
  exists <- doesFileExist freezeFile
  if exists
    then decodeYaml freezeFile >>= either die (return . map toConstraint . removeAddSourceDependencies . dependencies)
    else return []
  where
    removeAddSourceDependencies = filter ((`notElem` addSourceDependencies) . name)
