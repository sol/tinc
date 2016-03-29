module Tinc.Package (
  Package(..)
, setAddSourceHash
, Version(..)
, showPackage
, parsePackage
, parseInstallPlan
) where

import           Data.List
import           Data.String

import           Tinc.Fail

data Package = Package {
  packageName :: String
, packageVersion :: Version
} deriving (Eq, Ord, Show)

setAddSourceHash :: String -> Package -> Package
setAddSourceHash hash (Package name (Version number _)) =
  Package name (Version number (Just hash))

data Version = Version {
  versionNumber :: String
, versionAddSourceHash :: Maybe String
} deriving (Eq, Ord, Show)

instance IsString Version where
  fromString version = Version version Nothing

showPackage :: Package -> String
showPackage (Package name version) = name ++ "-" ++ showVersion version

showVersion :: Version -> String
showVersion (Version v _) = v

parsePackage :: String -> Package
parsePackage s = case break (== '-') (reverse s) of
  (v, '-' : p) -> Package (reverse p) (Version (reverse v) Nothing)
  _ -> Package s (Version "" Nothing)

parseInstallPlan :: Fail m => String -> m [Package]
parseInstallPlan input = case lines input of
  "Resolving dependencies..." : what : packages | needsInstalls what -> return (parse packages)
  "Resolving dependencies..." : what : _ | alreadyInstalled what -> return []
  _ -> bug ("unexpected output from `cabal install --dry-run':\n\n  " ++ show input ++ "\n")
  where
    needsInstalls = ("the following would be installed" `isInfixOf`)
    alreadyInstalled = (== "All the requested packages are already installed:")
    parse = map parsePackage . concatMap (take 1 . words)
