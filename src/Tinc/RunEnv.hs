module Tinc.RunEnv (run) where
import qualified Control.Exception as Exception
import qualified System.Directory as Directory
import qualified System.Process as Process

import           Tinc.Env
import           Tinc.Facts
import           Tinc.Cabal

run :: Facts -> Tinc a -> IO a
run facts = runEnv Env {
  envDoesFileExist = Directory.doesFileExist
, envCopyFile = Directory.copyFile
, envReadCabal = \args -> uncurry Process.readProcess (cabal facts args) ""
, envTry = Exception.try
, envThrowM = Exception.throwIO
}
