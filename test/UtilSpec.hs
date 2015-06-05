module UtilSpec where

import           Test.Hspec
import           Data.Either.Compat

import           Util

spec :: Spec
spec = do
  describe "parseInstallPlan" $ do
    it "parses output from `cabal install --dry-run`" $ do
      output <- readFile "test/resources/cabal-1.22.4.0-dry-run.txt"
      parseInstallPlan output `shouldBe`
        [ "base-compat-0.8.2"
        , "base-orphans-0.3.2"
        , "tagged-0.7.3"
        , "generics-sop-0.1.1.2"
        , "getopt-generics-0.6.3"
        ]

  describe "lookupPackage" $ do
    let tagged = "tagged-0.7-ad0cd5417885ac66ae852f4c39d51376.conf"
        tagged073 = "tagged-0.7.3-8d0cd5417885ac66ae852f4c39d51376.conf"
        package = "tagged-0.7"

    it "returns a package file for a given package" $ do
      lookupPackage package [tagged] `shouldBe` Right (Just tagged)

    context "when a package is the prefix of another package" $ do
      it "can disambiguate them" $ do
        lookupPackage package [tagged073, tagged] `shouldBe` Right (Just tagged)

    context "when there is no package file for the given package" $ do
      it "returns nothing" $ do
        lookupPackage package [tagged073] `shouldBe` Right Nothing

    context "when there are multiple package configs for the package" $ do
      it "returns an error" $ do
        lookupPackage package [tagged, tagged] `shouldSatisfy` isLeft
