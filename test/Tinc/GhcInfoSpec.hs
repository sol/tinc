module Tinc.GhcInfoSpec (spec) where

import           Test.Hspec

import           Data.List

import           Tinc.Types
import           Tinc.GhcInfo

spec :: Spec
spec = do
  describe "getGhcInfo" $ do
    it "includes path to global package database" $ do
      info <- getGhcInfo
      path (ghcInfoGlobalPackageDB info) `shouldSatisfy` ("package.conf.d" `isSuffixOf`)
