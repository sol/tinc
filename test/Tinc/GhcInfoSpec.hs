module Tinc.GhcInfoSpec (spec) where

import           Test.Hspec

import           Data.List

import           Tinc.Types
import           Tinc.GhcInfo

spec :: Spec
spec = do
  describe "ghcInfo" $ do
    it "includes path to global package database" $ do
      info <- ghcInfo
      path (ghcInfoGlobalPackageDB info) `shouldSatisfy` ("package.conf.d" `isSuffixOf`)
