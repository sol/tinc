module Tinc.SetupSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Test.Hspec
import           Test.Mockery.Directory
import           System.Directory
import           System.FilePath

import           Tinc.Setup

spec :: Spec
spec = do
  describe "listPlugins" $ do
    it "lists plugins" $ do
      inTempDirectory $ do
        touch "tinc-foo"
        touch "tinc-bar"
        pluginsDir <- getCurrentDirectory
        plugins <- listPlugins pluginsDir
        plugins `shouldMatchList` [
            ("foo", pluginsDir </> "tinc-foo")
          , ("bar", pluginsDir </> "tinc-bar")
          ]
