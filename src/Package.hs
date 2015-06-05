module Package where

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
parseInstallPlan = map parsePackage . concatMap (take 1 . words) . drop 2 . lines

lookupPackage :: Package -> [FilePath] -> Either String (Maybe FilePath)
lookupPackage targetPackage packageFiles =
  case filter ((showPackage targetPackage ++ "-") `isPrefixOf`) packageFiles of
    [packageFile] -> return $ Just packageFile
    [] -> return Nothing
    multiple -> Left ("Package found multiple times: " ++ intercalate ", " multiple)
