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
  withEnvironment (("HOME", dir) : env) action

withUseNix :: Maybe String -> IO a -> IO a
withUseNix value action = do
  env <- filter ((/= "TINC_USE_NIX") . fst) <$> getEnvironment
  withEnvironment (tincUseNix ++ env) action
  where
    tincUseNix = maybe [] (return . (,) "TINC_USE_NIX") value

spec :: Spec
spec = before_ pending $ do
  describe "discoverFacts" $ around_ withTempHome $ do
    it "includes GHC version in cache directory" $ do
      Facts{..} <- discoverFacts "/some/path/to/tinc"
      path factsCache `shouldContain` ghcInfoVersion factsGhcInfo

    describe "factsUseNix" $ do
      context "when TINC_USE_NIX is not set" $ around_ (withUseNix Nothing) $ do
        context "when executable is installed under /nix" $ do
          it "is True" $ do
            Facts{..} <- discoverFacts "/nix/some/path/to/tinc"
            factsUseNix `shouldBe` True

        context "when executable is not installed under /nix" $ do
          it "is False" $ do
            Facts{..} <- discoverFacts "/some/path/to/tinc"
            factsUseNix `shouldBe` False

      context "when TINC_USE_NIX is set to 'yes'" $ around_ (withUseNix $ Just "yes") $ do
        context "when executable is installed under /nix" $ do
          it "is True" $ do
            Facts{..} <- discoverFacts "/nix/some/path/to/tinc"
            factsUseNix `shouldBe` True

        context "when executable is not installed under /nix" $ do
          it "is True" $ do
            Facts{..} <- discoverFacts "/some/path/to/tinc"
            factsUseNix `shouldBe` True


      context "when TINC_USE_NIX is set to 'no'" $ around_ (withUseNix $ Just "no") $ do
        context "when executable is installed under /nix" $ do
          it "is False" $ do
            Facts{..} <- discoverFacts "/nix/some/path/to/tinc"
            factsUseNix `shouldBe` False

        context "when executable is not installed under /nix" $ do
          it "is False" $ do
            Facts{..} <- discoverFacts "/some/path/to/tinc"
            factsUseNix `shouldBe` False


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
