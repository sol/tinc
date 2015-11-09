{-# LANGUAGE OverloadedStrings #-}
module Tinc.FreezeSpec (spec) where

import           Test.Hspec
import           Test.Mockery.Directory

import           Tinc.Freeze
import           Tinc.Package

spec :: Spec
spec = do
  describe "readFreezeFile" $ do
    it "returns constraints from freeze file" $ do
      inTempDirectory $ do
        writeFreezeFile [Package "hspec" "2.2.0"]
        readFreezeFile `shouldReturn` ["--constraint=hspec == 2.2.0"]

    context "without freeze file" $ do
      it "returns empty list" $ do
        inTempDirectory $ do
          readFreezeFile `shouldReturn` []
