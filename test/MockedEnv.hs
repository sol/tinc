{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MockedEnv where

import           Control.Monad.Catch
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class

import           Tinc.Fail

import           Control.Monad.Trans.Class

newtype WithEnv e a = WithEnv (ReaderT e IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance Fail (WithEnv e) where
  die = WithEnv . lift . die

withEnv :: e -> WithEnv e a -> IO a
withEnv e (WithEnv action) = runReaderT action e
