{-# LANGUAGE OverloadedStrings #-}
module Tinc.FreezeSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Test.Hspec
import           Test.Mockery.Directory
import           System.Posix.Files

import           Tinc.Freeze
import           Tinc.Package
import           Tinc.Sandbox

spec :: Spec
spec = around_ inTempDirectory $ do
  describe "writeFreezeFile" $ do
    context "when nothing has changed" $ do
      it "it does not update the file modification time" $ do
        let contents = [Package "hspec" "2.2.0"]
        writeFreezeFile contents
        let t0 = 0
        setFileTimes freezeFile t0 t0
        writeFreezeFile contents
        t1 <- modificationTime <$> getFileStatus "tinc.freeze"
        t1 `shouldBe` t0

  describe "readFreezeFile" $ do
    it "returns constraints from freeze file" $ do
      writeFreezeFile [Package "hspec" "2.2.0"]
      readFreezeFile [] `shouldReturn` ["--constraint=hspec == 2.2.0"]

    it "omits add-source dependencies" $ do
      writeFreezeFile [Package "HUnit" "1.4.0.0", Package "hspec" "2.2.0"]
      readFreezeFile [AddSource "HUnit" "some-rev"] `shouldReturn` ["--constraint=hspec == 2.2.0"]

    context "without freeze file" $ do
      it "returns empty list" $ do
        readFreezeFile [] `shouldReturn` []
