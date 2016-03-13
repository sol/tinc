module Tinc.SetupSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Test.Hspec
import           Test.Mockery.Directory
import           System.Directory
import           System.FilePath
import           System.Process

import           Tinc.Types
import           Tinc.GhcInfo
import           Tinc.Setup

mkExecutable :: FilePath -> IO ()
mkExecutable p = do
  touch p
  callProcess "chmod" ["+x", p]

spec :: Spec
spec = do
  describe "setup" $ beforeAll setup $ do
    it "includes GHC version in cache directory" $ \ facts -> do
      path (factsCache facts) `shouldContain` ghcInfoVersion (factsGhcInfo facts)

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
