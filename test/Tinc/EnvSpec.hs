{-# LANGUAGE FlexibleContexts #-}
module Tinc.EnvSpec (spec) where

import           Test.Hspec
import           Test.Mockery.Action
import           Data.WithLocation

import           Tinc.Env

env :: WithLocation (Env IO)
env = Env {
  envDoesFileExist = dummy "envDoesFileExist"
, envCopyFile = dummy "envCopyFile"
}

spec :: Spec
spec = do
  describe "copyFileIfExists" $ do
    it "copies a file" $ do
      expectOnce (stub ("foo", "bar", return ())) $ \copyFile_ -> do
        runEnv env { envDoesFileExist = stub ("foo", return True), envCopyFile = copyFile_ } $ do
          copyFileIfExists "foo" "bar"

    context "when file does not exist" $ do
      it "does nothing" $ do
        runEnv env { envDoesFileExist = stub ("foo", return False) } $ do
          copyFileIfExists "foo" "bar"
