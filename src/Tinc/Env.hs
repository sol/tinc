{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Tinc.Env where

import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Free.TH

import           Control.Exception (Exception)


data TincF next =
    DoesFileExist FilePath (Bool -> next)
  | CopyFile FilePath FilePath next
  | ReadCabal [String] (String -> next)
  | forall a e. Exception e => Try (Tinc a) (Either e a -> next)
  | forall e. Exception e => ThrowM e

deriving instance (Functor TincF)

type Tinc = Free TincF

makeFree ''TincF

data Env m = Env {
  envDoesFileExist :: FilePath -> m Bool
, envCopyFile :: FilePath -> FilePath -> m ()
, envReadCabal :: [String] -> m String
, envTry :: forall e a. Exception e => m a -> m (Either e a)
, envThrowM :: forall e a. Exception e => e -> m a
}

runEnv :: Monad m => Env m -> Tinc a -> m a
runEnv env@Env{..} = iterM go
  where
    go (DoesFileExist file next) = envDoesFileExist file >>= next
    go (CopyFile src dst next) = envCopyFile src dst >> next
    go (ReadCabal args next) = envReadCabal args >>= next
    go (Try action next) = envTry (runEnv env action) >>= next
    go (ThrowM e) = envThrowM e

copyFileIfExists :: FilePath -> FilePath -> Tinc ()
copyFileIfExists src dst = do
  exists <- doesFileExist src
  when exists $ do
    copyFile src dst
