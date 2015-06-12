{-# LANGUAGE RecordWildCards #-}
module Run where

import           System.Process
import           System.Exit.Compat

import           Tinc.Setup
import           Stack

tinc :: [String] -> IO ()
tinc args = do
  Facts{..} <- setup
  case args of
    [] -> installDependencies factsCache
    name : rest | Just plugin <- lookup name factsPlugins -> callProcess plugin rest
    _ -> die ("unrecognized arguments: " ++ show args)
