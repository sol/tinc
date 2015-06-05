module Util where

import           Data.List

type Package = String

parseInstallPlan :: String -> [Package]
parseInstallPlan = concatMap (take 1 . words) . drop 2 . lines

lookupPackage :: String -> [FilePath] -> Either String (Maybe FilePath)
lookupPackage targetPackage packageFiles =
  case filter ((targetPackage ++ "-") `isPrefixOf`) packageFiles of
    [packageFile] -> return $ Just packageFile
    [] -> return Nothing
    multiple -> Left ("Package found multiple times: " ++ intercalate ", " multiple)

isPackageDB :: FilePath -> Bool
isPackageDB = ("-packages.conf.d" `isSuffixOf`)
