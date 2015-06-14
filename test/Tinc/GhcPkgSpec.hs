{-# LANGUAGE CPP #-}
module Tinc.GhcPkgSpec (spec) where

import           Test.Hspec

import           Tinc.GhcPkg
import           Package

spec :: Spec
spec = do
  describe "listGlobalPackages" $ do
    it "lists packages from global package database" $ do
      packages <- listGlobalPackages
      map packageName packages `shouldMatchList` [
          "array"
        , "base"
        , "binary"
        , "bin-package-db"
        , "bytestring"
        , "Cabal"
        , "containers"
        , "deepseq"
        , "directory"
        , "filepath"
        , "ghc"
        , "ghc-prim"
        , "hoopl"
        , "hpc"
        , "integer-gmp"
        , "pretty"
        , "process"
        , "rts"
        , "template-haskell"
        , "time"
        , "unix"
#if __GLASGOW_HASKELL__ > 706
        , "haskeline"
        , "terminfo"
        , "transformers"
        , "xhtml"
#endif
#if __GLASGOW_HASKELL__ < 710
        , "haskell2010"
        , "haskell98"
        , "old-locale"
        , "old-time"
#endif
        ]
