{-# LANGUAGE DeriveGeneric #-}

module Run where

import qualified GHC.Generics as GHC
import           System.Console.GetOpt.Generics

import           Stack

data Options
  = Options {
    parent :: FilePath
  }
  deriving (GHC.Generic)

instance Generic Options
instance HasDatatypeInfo Options

run :: IO ()
run = do
  options <- getArguments
  createStackedSandbox (Path (parent options) :: Path Sandbox)
