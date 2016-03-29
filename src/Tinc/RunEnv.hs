module Tinc.RunEnv (run) where
import qualified Control.Exception as Exception
import qualified System.Directory as Directory
import qualified System.Process as Process
import qualified System.IO.Temp as Temp

import qualified Util
import           Tinc.Env
import           Tinc.Facts
import           Tinc.Cabal

run :: Facts -> Tinc a -> IO a
run facts = runEnv Env {
  envDoesFileExist = Directory.doesFileExist
, envCopyFile = Directory.copyFile
, envWithSystemTempDirectory = Temp.withSystemTempDirectory "tinc"
, envWithCurrentDirectory = Util.withCurrentDirectory

, envReadCabal = \args -> uncurry Process.readProcess (cabal facts args) ""

, envInitSandbox = undefined

, envTry = Exception.try
, envThrowM = Exception.throwIO
}
