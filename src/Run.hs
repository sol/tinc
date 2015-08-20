{-# LANGUAGE RecordWildCards #-}
module Run where

import           System.FileLock
import           System.FilePath
import           System.Process
import           System.Environment.Compat
import           Control.Exception

import           Tinc.Setup
import           Tinc.Install
import           Tinc.Types

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
    [] -> withCacheLock factsCache $
      installDependencies factsGhcInfo False factsCache factsGitCache
    ["--dry-run"] -> withCacheLock factsCache $
      installDependencies factsGhcInfo True factsCache factsGitCache
    name : rest | Just plugin <- lookup name factsPlugins -> callProcess plugin rest
    _ -> throwIO (ErrorCall $ "unrecognized arguments: " ++ show args)

withCacheLock :: Path CacheDir -> IO a -> IO a
withCacheLock cache action = do
  putStrLn $ "Acquiring " ++ lock
  withFileLock lock Exclusive $ \ _ -> action
  where
    lock = path cache </> "tinc.lock"
