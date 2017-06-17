{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Run where

import           Control.Exception
import           Control.Monad
import           Development.GitRev
import           System.Environment
import           System.FileLock
import           System.FilePath
import           System.Process

import           Tinc.Install
import           Tinc.Facts
import           Tinc.Types
import           Tinc.Nix
import           Tinc.RecentCheck

unsetEnvVars :: IO ()
unsetEnvVars = do
  unsetEnv "CABAL_SANDBOX_CONFIG"
  unsetEnv "CABAL_SANDBOX_PACKAGE_PATH"
  unsetEnv "GHC_PACKAGE_PATH"

tinc :: [String] -> IO ()
tinc args = do
  unsetEnvVars
  facts@Facts{..} <- getExecutablePath >>= discoverFacts
  case args of
    [] -> do
      withCacheLock facts $ do
        installDependencies False facts
    ["--fast"] -> do
      recent <- tincEnvCreationTime facts >>= isRecent
      unless recent $ do
        withCacheLock facts $ do
          installDependencies False facts
    ["--dry-run"] -> withCacheLock facts $
      installDependencies True facts
    ["--version"] -> putStrLn $(gitHash)
    "exec" : name : rest -> callExec facts name rest
    name : rest | Just plugin <- lookup name factsPlugins -> callPlugin plugin rest
    _ -> throwIO (ErrorCall $ "unrecognized arguments: " ++ show args)

callExec :: Facts -> String -> [String] -> IO ()
callExec Facts{..} name args = do
  let
    cmd
      | factsUseNix = nixShell name args
      | otherwise = ("cabal", "exec" : "--" : name : args)
  uncurry rawSystemExit cmd

callPlugin :: String -> [String] -> IO ()
callPlugin = rawSystemExit

rawSystemExit :: FilePath -> [String] -> IO ()
rawSystemExit path args = rawSystem path args >>= throwIO

withCacheLock :: Facts -> IO a -> IO a
withCacheLock Facts{..} action = do
  if factsContinuousIntegrationMode
    then do
      -- In CI-mode we do not lock the cache.  This is to prevent the cache
      -- from getting "dirty" when no packages need to be installed.
      putStrLn $ "NOTE: Running in CI-mode, not locking cache!"
      action
    else do
      putStrLn $ "Acquiring " ++ lock
      withFileLock lock Exclusive $ \ _ -> action
  where
    lock = path factsCache </> "tinc.lock"
