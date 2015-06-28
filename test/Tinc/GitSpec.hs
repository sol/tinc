{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Tinc.GitSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Helper
import           MockedEnv
import           Test.Mockery.Action

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Safe
import           System.Directory
import           System.FilePath
import qualified System.Process
import           Test.Mockery.Directory

import           Tinc.Git

data Env = Env {
  envReadProcess :: FilePath -> [String] -> String -> IO String
, envCallProcess :: FilePath -> [String] -> IO ()
}

env :: Env
env = Env System.Process.readProcess System.Process.callProcess

instance Process (WithEnv Env) where
  readProcess command args input = WithEnv $ asks envReadProcess >>= liftIO . ($ input) . ($ args) . ($ command)
  callProcess command args = WithEnv $ asks envCallProcess >>= liftIO . ($ args) . ($ command)

spec :: Spec
spec = do
  describe "clone" $ do
    let url = "https://github.com/haskell-tinc/hpack"
        rev = "6bebd90d1e22901e94460c02bba9d0fa5b343f81"

        mockedCallProcess command args = do
          let dst = atDef "/path/to/some/tmp/dir" args 2
              gitClone = ("git", ["clone", url, dst], createDirectory $ dst </> ".git")
              gitCheckout = ("git", ["reset", "--hard", "0.1.0"], writeFile "rev" (rev ++ "\n"))
          mockMany [gitClone, gitCheckout] command args

        mockedReadProcess = mock ("git", ["rev-parse", "HEAD"], "", readFile "rev")

        mockedEnv = env {envReadProcess = mockedReadProcess, envCallProcess = mockedCallProcess}

    context "with a tag" $ do
      let action = clone "git-cache" (GitDependency "hpack" url "0.1.0")

      it "adds specified git ref to cache" $ do
        inTempDirectory $ do
          actualRev <- withEnv mockedEnv action
          actualRev `shouldBe` GitRevision rev
          doesDirectoryExist ("git-cache" </> "hpack" </> rev) `shouldReturn` True
          doesDirectoryExist ("git-cache" </> "hpack" </> rev </> ".git") `shouldReturn` False

      it "is idempotent" $ do
        inTempDirectory $ do
          withEnv mockedEnv (action >> action) `shouldReturn` GitRevision rev

    context "with a git revision" $ do
      let action = clone "git-cache" (GitDependency "hpack" url rev)

      context "when the revision is already cached" $ do
        it "does nothing" $ do
          inTempDirectory $ do
            createDirectoryIfMissing True ("git-cache" </> "hpack" </> rev)
            withEnv env {envReadProcess = undefined, envCallProcess = undefined} action
              `shouldReturn` GitRevision rev
