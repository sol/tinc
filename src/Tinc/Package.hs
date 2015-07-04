{-# LANGUAGE ViewPatterns #-}
module Tinc.Package where

import           Data.List

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

parseInstallPlan :: String -> [Package]
parseInstallPlan (lines -> ("Resolving dependencies..."
                            : what
                            : maybePackages))
    | "the following would be installed" `isInfixOf` what
        = map parsePackage . concatMap (take 1 . words) $ maybePackages
parseInstallPlan _ = []


lookupPackage :: Package -> [FilePath] -> Either String (Maybe FilePath)
lookupPackage targetPackage packageFiles =
  case filter ((showPackage targetPackage ++ "-") `isPrefixOf`) packageFiles of
    [packageFile] -> return $ Just packageFile
    [] -> return Nothing
    multiple -> Left ("Package found multiple times: " ++ intercalate ", " multiple)
