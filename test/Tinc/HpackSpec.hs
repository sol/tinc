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
          , "    git: https://github.com/sol/hpack"
          , "    ref: master"
          , "  - bar"
          , "library: {}"
          ]
        extractGitDependencies `shouldReturn` [GitDependency "foo" "https://github.com/sol/hpack" "master"]

    it "extracts git dependencies from tinc.yaml" $ do
      inTempDirectory $ do
        writeFile "tinc.yaml" $ unlines [
            "dependencies:"
          , "  - name: foo"
          , "    git: https://github.com/sol/hpack"
          , "    ref: master"
          , "  - bar"
          , "library: {}"
          ]
        extractGitDependencies `shouldReturn` [GitDependency "foo" "https://github.com/sol/hpack" "master"]

    context "when the same git dependency is specified in both package.yaml and tinc.yaml" $ do
      it "gives tinc.yaml precedence" $ do
        inTempDirectory $ do
          writeFile "package.yaml" $ unlines [
              "dependencies:"
            , "  - name: foo"
            , "    git: https://github.com/sol/hpack"
            , "    ref: master"
            , "  - bar"
            , "library: {}"
            ]
          writeFile "tinc.yaml" $ unlines [
              "dependencies:"
            , "  - name: foo"
            , "    git: https://github.com/sol/hpack"
            , "    ref: dev"
            , "  - bar"
            , "library: {}"
            ]
          extractGitDependencies `shouldReturn` [GitDependency "foo" "https://github.com/sol/hpack" "dev"]

    context "when package.yaml can not be parsed" $ do
      it "throws an exception" $ do
        inTempDirectory $ do
          writeFile "package.yaml" $ unlines [
              "ghc-options: 23"
            , "library: {}"
            ]
          extractGitDependencies `shouldThrow` errorCall "package.yaml: when expecting a String, encountered Number instead"

    context "when package.yaml does not exist" $ do
      it "returns an empty list" $ do
        inTempDirectory $ do
          extractGitDependencies `shouldReturn` []
