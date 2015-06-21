{-# LANGUAGE RecordWildCards #-}
module Run where

import           System.Process
import           System.Environment.Compat
import           Control.Exception

import           Tinc.Setup
import           Tinc.Install

unsetEnvVars :: IO ()
unsetEnvVars = do
  unsetEnv "CABAL_SANDBOX_CONFIG"
  unsetEnv "CABAL_SANDBOX_PACKAGE_PATH"
  unsetEnv "GHC_PACKAGE_PATH"

tinc :: [String] -> IO ()
tinc args = do
  unsetEnvVars
  Facts{..} <- setup
  case args of
    [] -> installDependencies factsGhcInfo False factsCache
    ["--dry-run"] -> installDependencies factsGhcInfo True factsCache
    name : rest | Just plugin <- lookup name factsPlugins -> callProcess plugin rest
    _ -> throwIO (ErrorCall $ "unrecognized arguments: " ++ show args)
