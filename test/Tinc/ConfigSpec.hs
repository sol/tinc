{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Tinc.ConfigSpec where

import           Test.Hspec
import           Test.Mockery.Directory

import           Tinc.SourceDependencySpec (anyVersion)

import           Tinc.Config

spec :: Spec
spec = do
  describe "getAdditionalDependencies" $ do
    it "return additional dependencies from tinc.yaml" $ do
      inTempDirectory $ do
        writeFile "tinc.yaml" $ unlines [
            "dependencies:"
          , "  - foo"
          ]
        getAdditionalDependencies `shouldReturn` [("foo", anyVersion)]

    context "when tinc.yaml does not exist" $ do
      it "returns an empty list" $ do
        inTempDirectory $ do
          getAdditionalDependencies `shouldReturn` mempty
