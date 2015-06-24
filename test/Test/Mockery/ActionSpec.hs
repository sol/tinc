module Test.Mockery.ActionSpec (spec) where

import           Test.Hspec

import           Test.HUnit.Lang
import           Test.Mockery.Action

hUnitFailure :: String -> HUnitFailure -> Bool
hUnitFailure actual (HUnitFailure expected) = actual == expected

spec :: Spec
spec = do
  describe "mock" $ do
    context "with one parameter" $ do
      context "when receiving specified parameters" $ do
        it "returns specified value" $ do
          mock ("foo", return "r") "foo" `shouldReturn` "r"

      context "when receiving unexpected parameters" $ do
        it "throws an exception" $ do
          mock ("foo", return "r") "bar" `shouldThrow` (hUnitFailure . unlines) [
              "Unexected parameter to mocked action!"
            , "expected: " ++ show "foo"
            , " but got: " ++ show "bar"
            ]

    context "with two parameters" $ do
      context "when receiving specified parameters" $ do
        it "returns specified value" $ do
          mock ("foo", "bar", return "r") "foo" "bar" `shouldReturn` "r"

      context "when receiving unexpected parameters" $ do
        it "throws an exception" $ do
          mock ("foo", "bar", return "r") "23" "42" `shouldThrow` (hUnitFailure . unlines) [
              "Unexected parameters to mocked action!"
            , "expected: " ++ show ("foo", "bar")
            , " but got: " ++ show ("23", "42")
            ]

    context "with three parameters" $ do
      context "when receiving specified parameters" $ do
        it "returns specified value" $ do
          mock ("foo", "bar", "baz", return "r") "foo" "bar" "baz" `shouldReturn` "r"

      context "when receiving unexpected parameters" $ do
        it "throws an exception" $ do
          mock ("foo", "bar", "baz", return "r") "23" "42" "65" `shouldThrow` (hUnitFailure . unlines) [
              "Unexected parameters to mocked action!"
            , "expected: " ++ show ("foo", "bar", "baz")
            , " but got: " ++ show ("23", "42", "65")
            ]

  describe "mockMany" $ do
    context "with two parameters" $ do
      context "when receiving specified parameters" $ do
        it "returns specified value" $ do
          mockMany [("foo", "bar", return "r"), ("foo", "baz", return "_")] "foo" "bar" `shouldReturn` "r"

      context "when receiving unexpected parameters" $ do
        it "throws an exception" $ do
          mockMany [(10, 20, return ()), (23, 42, return ())] (23 :: Int) (65 :: Int) `shouldThrow` (hUnitFailure . unlines) [
              "Unexected parameters to mocked action!"
            , "expected one of: (10,20), (23,42)"
            , "        but got: (23,65)"
            ]
