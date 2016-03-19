module Tinc.Ghci where

import           Data.Maybe
import           Data.List
import           System.Directory
import           System.Process

ghci :: [String] -> IO ()
ghci args = do
  dumpOutput <- readProcess "cabal" ["exec", "--", "ghc-pkg", "dump"] ""
  ghciSpec args (extractIncludeDirs dumpOutput) >>= uncurry callProcess

ghciSpec :: [String] -> [FilePath] -> IO (String, [String])
ghciSpec args includeDirs = do
  exists <- doesFileExist cabalMacros
  let cabalMacrosArgs
        | exists = ["-optP-include" ++ cabalMacros]
        | otherwise = []
  return ("cabal", cabalArgs ++ cabalMacrosArgs ++ map ("-I" ++) includeDirs ++ args)
  where
    cabalMacros = "dist/build/autogen/cabal_macros.h"
    cabalArgs = ["exec", "--", "ghci", "-idist/build/", "-idist/build/autogen"]

extractIncludeDirs :: String -> [FilePath]
extractIncludeDirs = mapMaybe (stripPrefix "include-dirs: ") . lines
