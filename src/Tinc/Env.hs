{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Tinc.Env where

import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Free.TH

import           Control.Exception (Exception, ErrorCall(..))

import           Tinc.Types
import           Tinc.GhcPkg
import           Tinc.Fail
import           Tinc.Sandbox


data TincF next =
    DoesFileExist FilePath (Bool -> next)
  | CopyFile FilePath FilePath next
  | forall a. WithSystemTempDirectory (FilePath -> Tinc a) (a -> next) -- FIXME
  | forall a. WithCurrentDirectory FilePath (Tinc a) (a -> next)

  -- | CallCabal [String] next
  | ReadCabal [String] (String -> next)

  | InitSandbox [Path AddSource] [Path PackageConfig] (Path PackageDb -> next)

  | forall a e. Exception e => Try (Tinc a) (Either e a -> next)
  | forall e. Exception e => ThrowM e

deriving instance (Functor TincF)

type Tinc = Free TincF

makeFree ''TincF

instance Fail Tinc where
  die = throwM . ErrorCall

data Env m = Env {
  envDoesFileExist :: FilePath -> m Bool
, envCopyFile :: FilePath -> FilePath -> m ()
, envWithSystemTempDirectory :: forall a. (FilePath -> m a) -> m a
, envWithCurrentDirectory :: forall a. FilePath -> m a -> m a

-- , envCallCabal :: [String] -> m ()
, envReadCabal :: [String] -> m String

, envInitSandbox :: [Path AddSource] -> [Path PackageConfig] -> m (Path PackageDb)

, envTry :: forall e a. Exception e => m a -> m (Either e a)
, envThrowM :: forall e a. Exception e => e -> m a
}

runEnv :: Monad m => Env m -> Tinc a -> m a
runEnv env@Env{..} = iterM go
  where
    go (DoesFileExist file next) = envDoesFileExist file >>= next
    go (CopyFile src dst next) = envCopyFile src dst >> next
    go (WithSystemTempDirectory action next) = envWithSystemTempDirectory (\dir -> runEnv env (action dir)) >>= next
    go (WithCurrentDirectory dir action next) = envWithCurrentDirectory dir (runEnv env action) >>= next

    -- go (CallCabal args next) = envCallCabal args >> next
    go (ReadCabal args next) = envReadCabal args >>= next
    go (InitSandbox addSourceDependencies packageConfigs next) = envInitSandbox addSourceDependencies packageConfigs >>= next

    go (Try action next) = envTry (runEnv env action) >>= next
    go (ThrowM e) = envThrowM e

copyFileIfExists :: FilePath -> FilePath -> Tinc ()
copyFileIfExists src dst = do
  exists <- doesFileExist src
  when exists $ do
    copyFile src dst
