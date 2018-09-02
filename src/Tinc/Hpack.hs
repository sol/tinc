{-# LANGUAGE OverloadedLists #-}
module Tinc.Hpack (
  readConfig
, doesConfigExist
, render
, mkPackage
) where

import           System.Directory hiding (getDirectoryContents)
import qualified Hpack.Config as Hpack
import qualified Hpack.Render as Hpack

import           Tinc.Fail

doesConfigExist :: IO Bool
doesConfigExist = doesFileExist Hpack.packageConfig

readConfig :: Hpack.Dependencies -> IO (String, Hpack.Package)
readConfig additionalDeps = do
  r <- Hpack.readPackageConfig Hpack.defaultDecodeOptions >>= either die return
  return $ (Hpack.decodeResultCabalVersion r, addDependencies (Hpack.decodeResultPackage r))
  where
    addDependencies :: Hpack.Package -> Hpack.Package
    addDependencies p
      | additionalDeps == mempty = p
      | otherwise = p {Hpack.packageExecutables = [mkExecutable additionalDeps] <> Hpack.packageExecutables p}

render :: (String, Hpack.Package) -> (FilePath, String)
render (cabalVersion, pkg) = (name, cabalVersion ++ contents)
  where
    name :: String
    name = Hpack.packageName pkg ++ ".cabal"

    contents :: String
    contents = Hpack.renderPackageWith Hpack.defaultRenderSettings 2 [] [] pkg

mkPackage :: Hpack.Dependencies -> (String, Hpack.Package)
mkPackage deps = ("cabal-version: >= 1.10\n", (Hpack.package "tinc-generated" "0.0.0"){Hpack.packageExecutables = [mkExecutable deps]})

mkExecutable :: Hpack.Dependencies -> (String, Hpack.Section Hpack.Executable)
mkExecutable deps =
  ("tinc-generated", (Hpack.section $ Hpack.Executable (Just "Generated.hs") [] []){Hpack.sectionDependencies = deps})
