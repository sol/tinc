{-# LANGUAGE CPP #-}
module Test.Mockery.ActionSpec (spec) where

import           Test.Hspec
import           Control.Monad

import           Test.HUnit.Lang
import           Test.Mockery.Action

hUnitFailure :: String -> HUnitFailure -> Bool
hUnitFailure actual (HUnitFailure _ reason) = case reason of
  Reason expected -> actual == expected
  _ -> False

spec :: Spec
spec = do
  describe "dummy" $ do
    it "fails" $ do
      (dummy "test" :: Int -> Int -> IO Int) 23 42 `shouldThrow` hUnitFailure "Unexpected call to dummy action: test"

  describe "stub" $ do
    context "with one parameter" $ do
      context "when receiving specified parameters" $ do
        it "returns specified value" $ do
          stub ("foo", return "r") "foo" `shouldReturn` "r"

      context "when receiving unexpected parameters" $ do
        it "throws an exception" $ do
          stub ("foo", return "r") "bar" `shouldThrow` (hUnitFailure . unlines) [
              "Unexected parameter to stubbed action!"
            , "expected: " ++ show "foo"
            , " but got: " ++ show "bar"
            ]

    context "with two parameters" $ do
      context "when receiving specified parameters" $ do
        it "returns specified value" $ do
          stub ("foo", "bar", return "r") "foo" "bar" `shouldReturn` "r"

      context "when receiving unexpected parameters" $ do
        it "throws an exception" $ do
          stub ("foo", "bar", return "r") "23" "42" `shouldThrow` (hUnitFailure . unlines) [
              "Unexected parameters to stubbed action!"
            , "expected: " ++ show ("foo", "bar")
            , " but got: " ++ show ("23", "42")
            ]

    context "with three parameters" $ do
      context "when receiving specified parameters" $ do
        it "returns specified value" $ do
          stub ("foo", "bar", "baz", return "r") "foo" "bar" "baz" `shouldReturn` "r"

      context "when receiving unexpected parameters" $ do
        it "throws an exception" $ do
          stub ("foo", "bar", "baz", return "r") "23" "42" "65" `shouldThrow` (hUnitFailure . unlines) [
              "Unexected parameters to stubbed action!"
            , "expected: " ++ show ("foo", "bar", "baz")
            , " but got: " ++ show ("23", "42", "65")
            ]

    context "when used with lists" $ do
      context "with two parameters" $ do
        context "when receiving specified parameters" $ do
          it "returns specified value" $ do
            stub [("foo", "bar", return "r"), ("foo", "baz", return "_")] "foo" "bar" `shouldReturn` "r"

        context "when receiving unexpected parameters" $ do
          it "throws an exception" $ do
            stub [(10, 20, return ()), (23, 42, return ())] (23 :: Int) (65 :: Int) `shouldThrow` (hUnitFailure . unlines) [
                "Unexected parameters to stubbed action!"
              , "expected one of: (10,20), (23,42)"
              , "        but got: (23,65)"
              ]

  describe "withMock" $ do
    let
      withMockSpec stubbedAction call = do
        context "when action is called once" $ do
          it "passes" $ do
            withMock stubbedAction $ \action -> do
              call action
            `shouldReturn` "r"

        context "when action is called multiple times" $ do
          it "fails" $ do
            withMock stubbedAction $ \action -> do
              replicateM_ 10 (call action)
            `shouldThrow` hUnitFailure "Expected to be called once, but it was called 10 times instead!"

        context "when action is not called" $ do
          it "fails" $ do
            withMock stubbedAction $ \_ -> do
              return ()
            `shouldThrow` hUnitFailure "Expected to be called once, but it was called 0 times instead!"

    context "with one parameter" $ do
      let stubbedAction = stub ("foo", return "r")
          call action = action "foo"
      withMockSpec stubbedAction call

    context "with two parameters" $ do
      let stubbedAction = stub ("foo", "bar", return "r")
          call action = action "foo" "bar"
      withMockSpec stubbedAction call

    context "with three parameters" $ do
      let stubbedAction = stub ("foo", "bar", "baz", return "r")
          call action = action "foo" "bar" "baz"
      withMockSpec stubbedAction call

  describe "mockChain" $ do
    let actions = replicate 2 $ stub ("foo", "bar", return "r")

    context "when mock is called the specified number of times" $ do
      it "passes" $ do
        mockChain actions $ \mock -> do
          replicateM_ 2 (mock "foo" "bar")

    context "when mock is called too often" $ do
      it "fails" $ do
        mockChain actions $ \mock -> do
          replicateM_ 3 (mock "foo" "bar")
        `shouldThrow` hUnitFailure "Expected to be called only 2 times, but it received an additional call!"

    context "when mock is not called often enough" $ do
      it "fails" $ do
        mockChain actions $ \mock -> do
          replicateM_ 1 (mock "foo" "bar")
        `shouldThrow` hUnitFailure "Expected to be called 2 times, but it was called 1 time instead!"
