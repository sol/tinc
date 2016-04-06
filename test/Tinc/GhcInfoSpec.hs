{-# LANGUAGE CPP #-}
module Tinc.GhcInfoSpec (spec) where

import           Test.Hspec

import           Data.List
import           System.Info

import           Tinc.Types
import           Tinc.GhcInfo

spec :: Spec
spec = do
  describe "getGhcInfo" $ do
    beforeAll getGhcInfo $ do
      it "includes the target platform" $ \ ghcInfo -> do
        ghcInfoPlatform ghcInfo `shouldStartWith` arch

      it "includes the GHC version" $ \ ghcInfo -> do
        let major = [head $ show (__GLASGOW_HASKELL__ :: Int)]
        ghcInfoVersion ghcInfo `shouldSatisfy` (major `isPrefixOf`)

      it "includes the path to the global package database" $ \ ghcInfo -> do
        path (ghcInfoGlobalPackageDb ghcInfo) `shouldEndWith` "package.conf.d"
