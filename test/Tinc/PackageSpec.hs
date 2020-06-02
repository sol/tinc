{-# LANGUAGE OverloadedStrings #-}
module Tinc.PackageSpec where

import           Helper

import           Tinc.Package

spec :: Spec
spec = do
  describe "showPackage" $ do
    it "ignores add-source hash" $ do
      showPackage (Package "foo" "0.1.0" {versionAddSourceHash = Just "32509a18bb6ddc01014863d135a247bd65d16c38"})
        `shouldBe` "foo-0.1.0"

  describe "showPackageDetailed" $ do
    it "includes add-source hash" $ do
      showPackageDetailed (Package "foo" "0.1.0" {versionAddSourceHash = Just "32509a18bb6ddc01014863d135a247bd65d16c38"})
        `shouldBe` "foo-0.1.0 (32509a18bb6ddc01014863d135a247bd65d16c38)"

  describe "parseInstallPlan" $ do
    it "parses output from `cabal install --dry-run`" $ do
      output <- readFile "test/resources/cabal-1.22.4.0-dry-run.txt"
      parseInstallPlan output `shouldReturn` [
          SimplePackage "base-compat" "0.8.2"
        , SimplePackage "base-orphans" "0.3.2"
        , SimplePackage "tagged" "0.7.3"
        , SimplePackage "generics-sop" "0.1.1.2"
        , SimplePackage "getopt-generics" "0.6.3"
        ]

    context "when there is nothing to install" $ do
      it "returns an empty list" $ do
        output <- readFile "test/resources/cabal-1.22.4.0-dry-run-all-already-installed.txt"
        parseInstallPlan output `shouldReturn` []

    context "on unexpected input" $ do
      it "throws an exception" $ do
        parseInstallPlan "foo" `shouldThrow` (errorCall . unlines) [
            "src/Tinc/Package.hs: unexpected output from `cabal v1-install --dry-run':"
          , ""
          , "  \"foo\""
          , ""
          , "This is most likely a bug.  Please report an issue at:"
          , ""
          , "  https://github.com/sol/tinc/issues"
          ]

  describe "parsePackage" $ do
    it "parses packages" $ do
      parsePackage "foo-bar-1.2.3" `shouldBe` SimplePackage "foo-bar" "1.2.3"

    context "when package has no version" $ do
      it "returns package without version" $ do
        parsePackage "foo" `shouldBe` SimplePackage "foo" ""
