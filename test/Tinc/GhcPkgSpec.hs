module Tinc.GhcPkgSpec (spec) where

import           Control.Monad
import           System.Environment
import           Helper

import           Tinc.Facts
import           Tinc.GhcPkg
import           Tinc.Package

globalPackages :: [String]
globalPackages = [
    "array"
  , "base"
  , "binary"
  , "bytestring"
  , "Cabal"
  , "containers"
  , "deepseq"
  , "directory"
  , "filepath"
  , "ghc"
  , "ghc-boot"
  , "ghc-boot-th"
  , "ghci"
  , "ghc-prim"
  , "haskeline"
  , "hpc"
  , "integer-gmp"
  , "pretty"
  , "process"
  , "rts"
  , "template-haskell"
  , "terminfo"
  , "time"
  , "transformers"
  , "unix"
  , "xhtml"
  , "ghc-compact"
  , "mtl"
  , "parsec"
  , "stm"
  , "text"
  ]

spec :: Spec
spec = do
  describe "listGlobalPackages" $ before_ (getExecutablePath >>= useNix >>= (`when` pending)) $ do
    it "lists packages from global package database" $ do
      packages <- listGlobalPackages
      map simplePackageName packages `shouldMatchList` globalPackages
