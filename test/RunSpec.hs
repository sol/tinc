module RunSpec (spec) where

import           Helper
import           System.Exit

import           Run

spec :: Spec
spec = do
  describe "callPlugin" $ do
    it "propagates success" $ do
      callPlugin "true" [] `shouldThrow` (== ExitSuccess)

    it "propagates error" $ do
      callPlugin "false" [] `shouldThrow` (== ExitFailure 1)
