{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Tinc.HpackSpec (spec) where

import           Helper
import           Hpack.Config as Hpack
import           Safe
import           System.Directory
import           System.FilePath
import           System.IO.Error
import           System.IO.Temp

import           MockedEnv
import           MockedProcess
import           Test.Mockery.Action
import           Tinc.Hpack
import           Tinc.Sandbox as Sandbox

spec :: Spec
spec = do
  describe "parseAddSourceDependencies" $ do
    it "extracts git dependencies from package.yaml" $ do
      inTempDirectory $ do
        writeFile "package.yaml" $ unlines [
            "dependencies:"
          , "  - name: foo"
          , "    git: https://github.com/sol/hpack"
          , "    ref: master"
          , "  - bar"
          , "library: {}"
          ]
        parseAddSourceDependencies [] `shouldReturn` [("foo", GitRef "https://github.com/sol/hpack" "master")]

    it "extracts local dependencies" $ do
      inTempDirectory $ do
        writeFile "package.yaml" $ unlines [
            "dependencies:"
          , "  - name: foo"
          , "    path: ../foo"
          , "  - bar"
          , "library: {}"
          ]
        parseAddSourceDependencies [] `shouldReturn` [("foo", Local "../foo")]

    it "extracts git dependencies from list of additional dependencies " $ do
      inTempDirectory $ do
        parseAddSourceDependencies [Dependency "foo" (Just $ GitRef "https://github.com/sol/hpack" "master"), "bar"] `shouldReturn`
          [("foo", GitRef "https://github.com/sol/hpack" "master")]

    context "when the same git dependency is specified in both package.yaml and tinc.yaml" $ do
      it "gives tinc.yaml precedence" $ do
        inTempDirectory $ do
          writeFile "package.yaml" $ unlines [
              "dependencies:"
            , "  - name: foo"
            , "    git: https://github.com/sol/hpack"
            , "    ref: master"
            , "  - bar"
            , "library: {}"
            ]
          parseAddSourceDependencies [Dependency "foo" (Just $ GitRef "https://github.com/sol/hpack" "dev"), "bar"] `shouldReturn`
            [("foo", GitRef "https://github.com/sol/hpack" "dev")]

    context "when package.yaml can not be parsed" $ do
      it "throws an exception" $ do
        inTempDirectory $ do
          writeFile "package.yaml" $ unlines [
              "ghc-options: 23"
            , "library: {}"
            ]
          parseAddSourceDependencies [] `shouldThrow` errorCall "package.yaml: Error in $['ghc-options']: failed to parse field ghc-options: expected String, encountered Number"

    context "when package.yaml does not exist" $ do
      it "returns an empty list" $ do
        inTempDirectory $ do
          parseAddSourceDependencies [] `shouldReturn` []

  describe "cacheAddSourceDep" $ do
    let url = "https://github.com/haskell-tinc/hpack"
        rev = "6bebd90d1e22901e94460c02bba9d0fa5b343f81"
        cachedGitDependency = AddSource "hpack" rev

        mockedCallProcess command args = do
          let
            dst = atDef "/path/to/some/tmp/dir" args 2

            gitClone = ("git", ["clone", url, dst], ) $ do
              createDirectory $ dst </> ".git"
              writeFile (dst </> "hpack.cabal") "name: hpack"

            gitReset = ("git", ["reset", "--hard", rev], return ())

          stub [gitClone, gitReset] command args

        mockedEnv = env {envCallProcess = mockedCallProcess}

    let action = cacheAddSourceDep "git-cache" "hpack" (Hpack.GitRef url rev)

    it "adds specified git ref to cache" $ do
      inTempDirectory $ do
        actualRev <- withEnv mockedEnv action
        actualRev `shouldBe` cachedGitDependency
        doesDirectoryExist ("git-cache" </> "hpack" </> rev) `shouldReturn` True
        doesDirectoryExist ("git-cache" </> "hpack" </> rev </> ".git") `shouldReturn` False

    it "is idempotent" $ do
      inTempDirectory $ do
        withEnv mockedEnv (action >> action) `shouldReturn` cachedGitDependency

    context "when a revision is already in the cache" $ do
      it "does nothing" $ do
        inTempDirectory $ do
          createDirectoryIfMissing True ("git-cache" </> "hpack" </> rev)
          withEnv env {envReadProcess = undefined, envCallProcess = undefined} action
            `shouldReturn` cachedGitDependency

  describe "gitRefToRev" $ do
    let
      repo = "http://github.com/sol/with-location"

    it "resolves git references" $ do
      let
        ref = "master"
        output = "517c35a825cbb8eb53fadf4a24654f1227466155        refs/heads/master\n"
        mockedEnv = env {envReadProcess = stub ("git", ["ls-remote", repo, ref], "", return output)}
      withEnv mockedEnv (gitRefToRev repo ref) `shouldReturn` "517c35a825cbb8eb53fadf4a24654f1227466155"

    context "when git reference does not exist" $ do
      it "returns an error" $ do
        let
          ref = "master"
          output = ""
          mockedEnv = env {envReadProcess = stub ("git", ["ls-remote", repo, ref], "", return output)}
        withEnv mockedEnv (gitRefToRev repo ref) `shouldThrow` errorCall ("invalid reference " ++ show ref ++ " for git repository " ++ repo)

  describe "isGitRev" $ do
    context "when given a git revision" $ do
      it "it returns True" $ do
        isGitRev "cf3968b8c54a7204e4e73c04816d49317bad433d" `shouldBe` True

    context "when given a git reference" $ do
      it "it returns False" $ do
        isGitRev "master" `shouldBe` False

    context "when given a 40 character (160 bit) git reference" $ do
      it "it returns False" $ do
        isGitRev "very-long-branch-name-that-is-not-a-revi" `shouldBe` False

  describe "checkCabalName" $ do
    context "when git dependency name and cabal package name match" $ do
      it "succeeds" $ do
        withSystemTempDirectory "tinc" $ \ dir -> do
          let cabalFile = dir </> "foo.cabal"
          writeFile cabalFile "name: foo"
          checkCabalName dir "foo" (Hpack.GitRef "<url>" "<ref>")

    context "when git dependency name and cabal package name differ" $ do
      it "fails" $ do
        withSystemTempDirectory "tinc" $ \ dir -> do
          let cabalFile = dir </> "foo.cabal"
          writeFile cabalFile "name: foo"
          checkCabalName dir "bar" (Hpack.GitRef "<url>" "<ref>")
            `shouldThrow` errorCall "the git repository <url> contains package \"foo\", expected: \"bar\""

  describe "determinePackageName" $ do
    it "complains about invalid cabal files" $ do
      withSystemTempDirectory "tinc" $ \ dir -> do
        let cabalFile = dir </> "foo.cabal"
        writeFile cabalFile "library\n  build-depends: foo bar"
        determinePackageName dir (Hpack.GitRef "<repo>" "<ref>") `shouldThrow` isUserError

  describe "findCabalFile" $ do
    it "finds cabal files in given directory" $ do
      withSystemTempDirectory "tinc" $ \ dir -> do
        let cabalFile = dir </> "foo.cabal"
        touch cabalFile
        findCabalFile dir (Hpack.GitRef "<repo>" "<ref>") `shouldReturn` cabalFile

    context "when there is no cabal file" $ do
      it "reports an error" $ do
        withSystemTempDirectory "tinc" $ \ dir -> do
          findCabalFile dir (Hpack.GitRef "<repo>" "<ref>") `shouldThrow` errorCall "Couldn't find .cabal file in git repository <repo>"

    context "when there are multiple cabal files" $ do
      it "reports an error" $ do
        withSystemTempDirectory "tinc" $ \ dir -> do
          touch (dir </> "foo.cabal")
          touch (dir </> "bar.cabal")
          findCabalFile dir (Hpack.GitRef "<repo>" "<ref>") `shouldThrow` errorCall "Multiple cabal files found in git repository <repo>"
