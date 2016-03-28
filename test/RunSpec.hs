module RunSpec (spec) where

import           Helper
import           System.Exit

import           Run

spec :: Spec
spec = do
  describe "callPlugin" $ do
    it "propagates success" $ do
      callPlugin facts "true" [] `shouldThrow` (== ExitSuccess)

    it "propagates error" $ do
      callPlugin facts "false" [] `shouldThrow` (== ExitFailure 1)
