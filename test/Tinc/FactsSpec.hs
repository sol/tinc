{-# LANGUAGE RecordWildCards #-}
module Tinc.FactsSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Test.Hspec
import           Test.Mockery.Directory
import           Test.Mockery.Environment
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Process
import           System.IO.Temp

import           Tinc.Types
import           Tinc.GhcInfo
import           Tinc.Facts

mkExecutable :: FilePath -> IO ()
mkExecutable p = do
  touch p
  callProcess "chmod" ["+x", p]

withTempHome :: IO () -> IO ()
withTempHome action = withSystemTempDirectory "hspec" $ \dir -> do
  env <- filter ((== "PATH") . fst) <$> getEnvironment
  withEnvironment (("HOME", dir) : env) $ do
    action

spec :: Spec
spec = do
  describe "discoverFacts" $ around_ withTempHome $ do
    it "includes GHC version in cache directory" $ do
      Facts{..} <- discoverFacts
      path factsCache `shouldContain` ghcInfoVersion factsGhcInfo

    it "sets factsUseNix to False" $ do
      Facts{..} <- discoverFacts
      factsUseNix `shouldBe` False

    context "when TINC_USE_NIX is set" $ do
      it "sets factsUseNix to True" $ do
        env <- getEnvironment
        withEnvironment (("TINC_USE_NIX", "yes") : env) $ do
          Facts{..} <- discoverFacts
          factsUseNix `shouldBe` True

  describe "listPlugins" $ do
    it "lists plugins" $ do
      inTempDirectory $ do
        mkExecutable "tinc-foo"
        mkExecutable "tinc-bar"
        pluginsDir <- getCurrentDirectory
        plugins <- listPlugins pluginsDir
        plugins `shouldMatchList` [
            ("foo", pluginsDir </> "tinc-foo")
          , ("bar", pluginsDir </> "tinc-bar")
          ]

    it "excludes files that are not executable" $ do
      inTempDirectory $ do
        touch "tinc-foo"
        (getCurrentDirectory >>= listPlugins) `shouldReturn` []

    context "when directory does not exist" $ do
      it "returns an empty list" $ do
        listPlugins "foobar" `shouldReturn` []

  describe "listPathPlugins" $ do
    it "lists plugins" $ do
      inTempDirectory $ do
        mkExecutable "tinc-foo"
        dir1 <- getCurrentDirectory
        inTempDirectory $ do
          mkExecutable "tinc-bar"
          dir2 <- getCurrentDirectory

          plugins <- listPathPlugins [dir1, dir2]
          plugins `shouldMatchList` [
              ("foo", dir1 </> "tinc-foo")
            , ("bar", dir2 </> "tinc-bar")
            ]

    it "gives first occurrence precedence" $ do
      inTempDirectory $ do
        mkExecutable "tinc-foo"
        dir1 <- getCurrentDirectory
        inTempDirectory $ do
          mkExecutable "tinc-foo"
          dir2 <- getCurrentDirectory

          plugins <- listPathPlugins [dir1, dir2]
          plugins `shouldMatchList` [
              ("foo", dir1 </> "tinc-foo")
            ]
