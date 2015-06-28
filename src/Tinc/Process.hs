module Tinc.Process where

import           Prelude ()
import           Prelude.Compat
import qualified System.Process

class (Functor m, Applicative m, Monad m) => Process m where
  readProcess :: FilePath -> [String] -> String -> m String
  callProcess :: FilePath -> [String] -> m ()

instance Process IO where
  readProcess = System.Process.readProcess
  callProcess = System.Process.callProcess
