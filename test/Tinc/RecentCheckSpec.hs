module Tinc.RecentCheckSpec (spec) where

import           Helper

import           Data.Time
import           System.Directory
import           System.PosixCompat.Files
import           Foreign.C.Types (CTime)

import           Tinc.RecentCheck

touchOlder :: FilePath -> IO ()
touchOlder name = do
  touch name
  setFileTimes name 0 0

touchNewer :: FilePath -> IO ()
touchNewer name = do
  touch name
  setFileTimes name 2 2

epochToUTCTime :: CTime -> IO UTCTime
epochToUTCTime t = inTempDirectory $ do
  touch "foo.txt"
  setFileTimes "foo.txt" t t
  getModificationTime "foo.txt"

spec :: Spec
spec = do
  describe "isRecent" $ around_ inTempDirectory $ do

    context "with existing tinc environment" $ do
      let envCreationTime = Just <$> epochToUTCTime 1

      context "when tinc.freeze is older" $ before_ (touchOlder "tinc.freeze") $ do
        it "returns True" $ do
          (envCreationTime >>= isRecent) `shouldReturn` True

        context "when cabal file is newer" $ do
          it "returns False" $ do
            touchNewer "foo.cabal"
            (envCreationTime >>= isRecent) `shouldReturn` False

      context "when tinc.freeze is newer" $ do
        it "returns False" $ do
          touchNewer "tinc.freeze"
          (envCreationTime >>= isRecent) `shouldReturn` False

      context "without tinc.freeze" $ do
        it "returns False" $ do
          (envCreationTime >>= isRecent) `shouldReturn` False

    context "without tinc environment" $ do
      it "returns False" $ do
        isRecent Nothing `shouldReturn` False
