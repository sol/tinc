{-# LANGUAGE FlexibleContexts #-}
module Tinc.EnvSpec (spec) where

import           Helper

import           Tinc.Env

spec :: Spec
spec = do
  describe "copyFileIfExists" $ do
    it "copies a file" $ do
      expectOnce (stub ("foo", "bar", return ())) $ \copyFile_ -> do
        runEnv dummyEnv { envDoesFileExist = stub ("foo", return True), envCopyFile = copyFile_ } $ do
          copyFileIfExists "foo" "bar"

    context "when file does not exist" $ do
      it "does nothing" $ do
        runEnv dummyEnv { envDoesFileExist = stub ("foo", return False) } $ do
          copyFileIfExists "foo" "bar"
