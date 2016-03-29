{-# LANGUAGE CPP #-}
module Tinc.GhcInfoSpec (spec) where

import           Test.Hspec

import           Data.List

import           Tinc.Types
import           Tinc.GhcInfo

arch :: String
#ifdef x86_64_HOST_ARCH
arch = "x86_64"
#else
#error add support for platform here
#endif

spec :: Spec
spec = do
  describe "getGhcInfo" $ do
    beforeAll (pendingWith "slow" >> getGhcInfo) $ do
      it "includes the target platform" $ \ ghcInfo -> do
        ghcInfoPlatform ghcInfo `shouldSatisfy` (arch `isPrefixOf`)

      it "includes the GHC version" $ \ ghcInfo -> do
        let major = [head $ show (__GLASGOW_HASKELL__ :: Int)]
        ghcInfoVersion ghcInfo `shouldSatisfy` (major `isPrefixOf`)

      it "includes the path to the global package database" $ \ ghcInfo -> do
        path (ghcInfoGlobalPackageDb ghcInfo) `shouldSatisfy` ("package.conf.d" `isSuffixOf`)
