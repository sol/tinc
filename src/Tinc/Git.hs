module Tinc.Git where

import           Prelude ()
import           Prelude.Compat

import           Data.String
import qualified System.Process
import           System.IO.Temp
import           System.FilePath
import           System.Directory
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Control.Monad.Compat

import           Util
import           Tinc.Types

data GitDependency = GitDependency {
  gitDependencyName :: String
, gitDependencyUrl :: String
, gitDependencyRef :: String
} deriving (Eq, Show)

class (Functor m, Applicative m, Monad m) => Process m where
  readProcess :: FilePath -> [String] -> String -> m String
  callProcess :: FilePath -> [String] -> m ()

instance Process IO where
  readProcess = System.Process.readProcess
  callProcess = System.Process.callProcess

data GitCache

newtype GitRevision = GitRevision String
  deriving (Eq, Show)

instance IsString GitRevision where
  fromString = GitRevision

clone :: (Process m, MonadIO m, MonadMask m) => Path GitCache -> GitDependency -> m GitRevision
clone (Path cache) (GitDependency name url ref) = do
  alreadyInCache <- liftIO $ doesDirectoryExist (cache </> name </> ref)
  GitRevision <$> if alreadyInCache then return ref else populateCache
  where
    populateCache = do
      withSystemTempDirectory "tinc" $ \sandbox -> do
        tmp <- liftIO $ createTempDirectory sandbox name

        callProcess "git" ["clone", url, tmp]
        rev <- withCurrentDirectory tmp $ do
          callProcess "git" ["reset", "--hard", ref]
          rev <- strip <$> readProcess "git" ["rev-parse", "HEAD"] ""
          liftIO $ removeDirectoryRecursive ".git"
          return rev

        liftIO $ do
          let dst = cache </> name </> rev
          exists <- doesDirectoryExist dst
          unless exists $ do
            createDirectoryIfMissing True (cache </> name)
            renameDirectory tmp dst

        return rev
