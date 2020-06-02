{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Tinc.Package (
  Package(..)
, Version(..)
, showPackage
, showPackageDetailed
, SimplePackage(..)
, parsePackage
, parseInstallPlan
) where

import           Data.List
import           Data.String
import           Data.Store
import           GHC.Generics

import           Tinc.Fail

data Package = Package {
  packageName :: String
, packageVersion :: Version
} deriving (Eq, Ord, Show)

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

showPackageDetailed :: Package -> String
showPackageDetailed (Package name version) = name ++ "-" ++ showVersionDetailed version

showVersionDetailed :: Version -> String
showVersionDetailed (Version v mHash) = v ++ maybe "" (\ hash -> " (" ++ hash ++ ")") mHash

data SimplePackage = SimplePackage {
  simplePackageName :: String
, simplePackageVersion :: String
} deriving (Eq, Ord, Show, Generic, Store)

parsePackage :: String -> SimplePackage
parsePackage s = case break (== '-') (reverse s) of
  (v, '-' : p) -> SimplePackage (reverse p) (reverse v)
  _ -> SimplePackage s ""

parseInstallPlan :: Fail m => String -> m [SimplePackage]
parseInstallPlan input = case lines input of
  "Resolving dependencies..." : what : packages | needsInstalls what -> return (parse packages)
  "Resolving dependencies..." : what : _ | alreadyInstalled what -> return []
  _ -> bug ("unexpected output from `cabal v1-install --dry-run':\n\n  " ++ show input ++ "\n")
  where
    needsInstalls = ("the following would be installed" `isInfixOf`)
    alreadyInstalled = (== "All the requested packages are already installed:")
    parse = map parsePackage . concatMap (take 1 . words)
