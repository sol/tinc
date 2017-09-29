module Tinc.Hpack (
  readConfig
, doesConfigExist
, render
, mkPackage
) where

import           System.Directory hiding (getDirectoryContents)
import qualified Hpack.Config as Hpack
import           Hpack.Run

import           Tinc.Fail

doesConfigExist :: IO Bool
doesConfigExist = doesFileExist Hpack.packageConfig

readConfig :: Hpack.Dependencies -> IO Hpack.Package
readConfig additionalDeps = Hpack.readPackageConfig Hpack.packageConfig >>= either die (return . addDependencies . snd)
  where
    addDependencies :: Hpack.Package -> Hpack.Package
    addDependencies p
      | additionalDeps == mempty = p
      | otherwise = (Hpack.renamePackage "tinc-generated" p) {Hpack.packageExecutables = mkExecutable additionalDeps : Hpack.packageExecutables p}

render :: Hpack.Package -> (FilePath, String)
render pkg = (name, contents)
  where
    name :: String
    name = Hpack.packageName pkg ++ ".cabal"

    contents :: String
    contents = renderPackage defaultRenderSettings 2 [] [] pkg

mkPackage :: Hpack.Dependencies -> Hpack.Package
mkPackage deps = (Hpack.package "tinc-generated" "0.0.0"){Hpack.packageExecutables = [mkExecutable deps]}

mkExecutable :: Hpack.Dependencies -> Hpack.Section Hpack.Executable
mkExecutable deps = (Hpack.section $ Hpack.Executable "tinc-generated" "Generated.hs" []){Hpack.sectionDependencies = deps}
