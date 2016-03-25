{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Tinc.Env where

import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Free.TH

import qualified System.Directory as Directory

data TincF a =
    DoesFileExist FilePath (Bool -> a)
  | CopyFile FilePath FilePath a
  deriving (Functor)

type Tinc = Free TincF

makeFree ''TincF


data Env m = Env {
  envDoesFileExist :: FilePath -> m Bool
, envCopyFile :: FilePath -> FilePath -> m ()
}

run :: Tinc a -> IO a
run = runEnv Env {
  envDoesFileExist = Directory.doesFileExist
, envCopyFile = Directory.copyFile
}

runEnv :: Monad m => Env m -> Tinc a -> m a
runEnv Env{..} = iterM go
  where
    go (DoesFileExist file next) = envDoesFileExist file >>= next
    go (CopyFile src dst next) = envCopyFile src dst >> next

copyFileIfExists :: FilePath -> FilePath -> Tinc ()
copyFileIfExists src dst = do
  exists <- doesFileExist src
  when exists $ do
    copyFile src dst
