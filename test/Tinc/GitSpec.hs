{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Tinc.GitSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Helper
import           MockedEnv
import           MockedProcess
import           Test.Mockery.Action

import           Safe
import           System.Directory
import           System.FilePath
import           System.IO.Error
import           System.IO.Temp
import           Test.Mockery.Directory

import           Tinc.Git
import           Tinc.Hpack
import           Tinc.Sandbox

spec :: Spec
spec = do
  describe "clone" $ do
    let url = "https://github.com/haskell-tinc/hpack"
        rev = "6bebd90d1e22901e94460c02bba9d0fa5b343f81"
        cachedGitDependency = AddSource "hpack" rev

        mockedCallProcess command args = do
          let dst = atDef "/path/to/some/tmp/dir" args 2
              gitClone = ("git", ["clone", url, dst], ) $ do
                createDirectory $ dst </> ".git"
                writeFile (dst </> "hpack.cabal") "name: hpack"
              gitCheckout = ("git", ["reset", "--hard", "0.1.0"], writeFile "rev" (rev ++ "\n"))
          mockMany [gitClone, gitCheckout] command args

        mockedReadProcess = mock ("git", ["rev-parse", "HEAD"], "", readFile "rev")

        mockedEnv = env {envReadProcess = mockedReadProcess, envCallProcess = mockedCallProcess}

    context "with a tag" $ do
      let action = clone "git-cache" (GitDependency "hpack" url "0.1.0")

      it "adds specified git ref to cache" $ do
        inTempDirectory $ do
          actualRev <- withEnv mockedEnv action
          actualRev `shouldBe` cachedGitDependency
          doesDirectoryExist ("git-cache" </> "hpack" </> rev) `shouldReturn` True
          doesDirectoryExist ("git-cache" </> "hpack" </> rev </> ".git") `shouldReturn` False

      it "is idempotent" $ do
        inTempDirectory $ do
          withEnv mockedEnv (action >> action) `shouldReturn` cachedGitDependency

    context "with a git revision" $ do
      let action = clone "git-cache" (GitDependency "hpack" url rev)

      context "when the revision is already cached" $ do
        it "does nothing" $ do
          inTempDirectory $ do
            createDirectoryIfMissing True ("git-cache" </> "hpack" </> rev)
            withEnv env {envReadProcess = undefined, envCallProcess = undefined} action
              `shouldReturn` cachedGitDependency

  describe "checkCabalName" $ do
    context "when git dependency name and cabal package name match" $ do
      it "succeeds" $ do
        withSystemTempDirectory "tinc" $ \ dir -> do
          let cabalFile = dir </> "foo.cabal"
          writeFile cabalFile "name: foo"
          checkCabalName dir (GitDependency "foo" "<url>" "<ref>")

    context "when git dependency name and cabal package name differ" $ do
      it "fails" $ do
        withSystemTempDirectory "tinc" $ \ dir -> do
          let cabalFile = dir </> "foo.cabal"
          writeFile cabalFile "name: foo"
          checkCabalName dir (GitDependency "bar" "<url>" "<ref>")
            `shouldThrow` errorCall "the git repository <url> contains package \"foo\", expected: \"bar\""

  describe "determinePackageName" $ do
    it "complains about invalid cabal files" $ do
      withSystemTempDirectory "tinc" $ \ dir -> do
        let cabalFile = dir </> "foo.cabal"
        writeFile cabalFile "library\n  build-depends: foo bar"
        determinePackageName dir "<repo>" `shouldThrow` isUserError

  describe "getCabalFile" $ do
    it "finds cabal files in given directory" $ do
      withSystemTempDirectory "tinc" $ \ dir -> do
        let cabalFile = dir </> "foo.cabal"
        touch cabalFile
        findCabalFile dir "<repo>" `shouldReturn` cabalFile

    context "when there is no cabal file" $ do
      it "reports an error" $ do
        withSystemTempDirectory "tinc" $ \ dir -> do
          findCabalFile dir "<repo>" `shouldThrow` errorCall "Couldn't find .cabal file in git repository: <repo>"

    context "when there are multiple cabal files" $ do
      it "reports an error" $ do
        withSystemTempDirectory "tinc" $ \ dir -> do
          touch (dir </> "foo.cabal")
          touch (dir </> "bar.cabal")
          findCabalFile dir "<repo>" `shouldThrow` errorCall "Multiple cabal files found in git repository: <repo>"
