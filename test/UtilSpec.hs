module UtilSpec where

import           System.Directory
import           Test.Hspec
import           Test.Mockery.Directory

import           Util

spec :: Spec
spec = do
  describe "listDirectories" $ do
    it "lists directories" $ do
      inTempDirectory $ do
        createDirectory "foo"
        createDirectory "bar"
        writeFile "baz" ""
        listDirectories "." `shouldReturn` ["./bar", "./foo"]
