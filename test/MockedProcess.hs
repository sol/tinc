{-# LANGUAGE FlexibleInstances #-}
module MockedProcess where

import MockedEnv

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Tinc.Process

type ReadProcess = FilePath -> [String] -> String -> IO String
type CallProcess = FilePath -> [String] -> IO ()

data Env = Env {
  envReadProcess :: ReadProcess
, envCallProcess :: CallProcess
}

env :: Env
env = Env readProcess callProcess

instance Process (WithEnv Env) where
  readProcess command args input = WithEnv $ asks envReadProcess >>= liftIO . ($ input) . ($ args) . ($ command)
  callProcess command args = WithEnv $ asks envCallProcess >>= liftIO . ($ args) . ($ command)
