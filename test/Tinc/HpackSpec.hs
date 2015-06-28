module Tinc.HpackSpec (spec) where

import           Helper
import           Test.Mockery.Directory

import           Tinc.Hpack

spec :: Spec
spec = do
  describe "extractGitDependencies" $ do
    it "extracts git dependencies from package.yaml" $ do
      inTempDirectory $ do
        writeFile "package.yaml" $ unlines [
            "dependencies:"
          , "  - name: foo"
          , "    git:"
          , "      url: https://github.com/sol/hpack"
          , "      ref: master"
          , "  - bar"
          , "library: {}"
          ]
        extractGitDependencies `shouldReturn` [GitDependency "foo" "https://github.com/sol/hpack" "master"]

    context "when package.yaml can not be parsed" $ do
      it "throws an exception" $ do
        inTempDirectory $ do
          writeFile "package.yaml" $ unlines [
              "ghc-options: 23"
            , "library: {}"
            ]
          extractGitDependencies `shouldThrow` errorCall "package.yaml: when expecting a String, encountered Number instead"
