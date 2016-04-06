module Tinc.Process where

import           Prelude ()
import           Prelude.Compat
import qualified System.Process

process :: Process IO
process = Process {
  readProcess = System.Process.readProcess
, callProcess = System.Process.callProcess
}

data Process m = Process {
  readProcess :: FilePath -> [String] -> String -> m String
, callProcess :: FilePath -> [String] -> m ()
}

class (Functor m, Applicative m, Monad m) => MonadProcess m where
  readProcessM :: FilePath -> [String] -> String -> m String
  callProcessM :: FilePath -> [String] -> m ()

instance MonadProcess IO where
  readProcessM = System.Process.readProcess
  callProcessM = System.Process.callProcess
