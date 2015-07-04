{-# LANGUAGE CPP #-}
module Tinc.Package where

import           Data.List

import           Tinc.Fail

data Package
  = Package {
    packageName :: String,
    packageVersion :: String
  }
  deriving (Eq, Ord, Show)

showPackage :: Package -> String
showPackage (Package name version) = name ++ "-" ++ version

parsePackage :: String -> Package
parsePackage s = case break (== '-') (reverse s) of
  (v, '-' : p) -> Package (reverse p) (reverse v)
  _ -> Package s ""

parseInstallPlan :: Fail m => String -> m [Package]
parseInstallPlan input = case lines input of
  "Resolving dependencies..." : what : packages | needsInstalls what -> return (parse packages)
  "Resolving dependencies..." : what : _ | alreadyInstalled what -> return []
  _ -> bug __FILE__ ("unexpected output from `cabal install --dry-run':\n\n  " ++ show input ++ "\n")
  where
    needsInstalls = ("the following would be installed" `isInfixOf`)
    alreadyInstalled = (== "All the requested packages are already installed:")
    parse = map parsePackage . concatMap (take 1 . words)
