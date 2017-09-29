{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Tinc.AddSourceSpec (spec) where

import           Helper
import           Hpack.Config as Hpack hiding (Local)
import           Data.Tree
import           Data.Version
import           System.Directory
import           System.FilePath
import           System.IO.Error
import           System.IO.Temp
import           Test.QuickCheck hiding (output)
import           GHC.Fingerprint

import           Test.Mockery.Action
import           Tinc.Types
import           Tinc.AddSource

spec :: Spec
spec = do
  describe "mapLocalDependencyToGitDependency" $ do
    context "with a git dependency" $ do
      it "is the identity" $ do
        let dep = AddSourceDependency "bar" (Git "sol/bar" "some-rev" Nothing) :: AddSourceDependency Ref
        mapLocalDependencyToGitDependency undefined dep `shouldBe` dep

    context "with a local dependency" $ do
      let dep = AddSourceDependency "bar" (Local "./bar") :: AddSourceDependency Ref
      context "when source is Git" $ do
        it "maps the local dependency to a git dependency" $ do
          let
            source = Git "sol/foo" "some-rev" Nothing
            expected = AddSourceDependency "bar" (Git "sol/foo" "some-rev" (Just "bar"))
          mapLocalDependencyToGitDependency source dep `shouldBe` expected

        it "takes the subdir of the git dependency into account" $ do
          let
            source = Git "sol/foo" "some-rev" (Just "foo")
            expected = AddSourceDependency "bar" (Git "sol/foo" "some-rev" (Just "foo/bar"))
          mapLocalDependencyToGitDependency source dep `shouldBe` expected

      context "when source is Local" $ do
        it "makes the path of the dependency relative to the source" $ do
          let
            source = Local "../packages/foo"
            expected = AddSourceDependency "bar" (Local "../packages/foo/bar")
          mapLocalDependencyToGitDependency source dep `shouldBe` expected

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
        parseAddSourceDependencies [] `shouldReturn` [AddSourceDependency "foo" (Git "https://github.com/sol/hpack" "master" Nothing)]

    it "extracts local dependencies" $ do
      inTempDirectory $ do
        writeFile "package.yaml" $ unlines [
            "dependencies:"
          , "  - name: foo"
          , "    path: ../foo"
          , "  - bar"
          , "library: {}"
          ]
        parseAddSourceDependencies [] `shouldReturn` [AddSourceDependency "foo" (Local "../foo")]

    it "extracts git dependencies from list of additional dependencies " $ do
      inTempDirectory $ do
        parseAddSourceDependencies [("foo", SourceDependency $ GitRef "https://github.com/sol/hpack" "master" Nothing), ("bar", AnyVersion)] `shouldReturn`
          [AddSourceDependency "foo" (Git "https://github.com/sol/hpack" "master" Nothing)]

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
          parseAddSourceDependencies [("foo", SourceDependency $ GitRef "https://github.com/sol/hpack" "dev" Nothing), ("bar", AnyVersion)] `shouldReturn`
            [AddSourceDependency "foo" (Git "https://github.com/sol/hpack" "dev" Nothing)]

    context "when package.yaml can not be parsed" $ do
      it "throws an exception" $ do
        inTempDirectory $ do
          writeFile "package.yaml" $ unlines [
              "ghc-options: 23"
            , "library: {}"
            ]
          parseAddSourceDependencies [] `shouldThrow` errorCall "package.yaml: Error in $['ghc-options']: expected String, encountered Number"

    context "when package.yaml does not exist" $ do
      it "returns an empty list" $ do
        inTempDirectory $ do
          parseAddSourceDependencies [] `shouldReturn` []

  describe "populateAddSourceCache_impl" $ around_ inTempDirectory $ do
    let
      name = "hpack"
      url = "https://github.com/sol/hpack"
      rev = "6bebd90d1e22901e94460c02bba9d0fa5b343f81"


      cacheAddSourceDepSpec subdir cacheKey cabalFile = do
        let
          gitDependency = Git url (CachedRev rev) subdir

          cache :: Path AddSourceCache
          cache = "add-source"

          gitCache :: Path GitCache
          gitCache = "git-cache"

          cachedGitDependency = AddSource name cacheKey
          cachedGitDependencyPath = addSourcePath cache cachedGitDependency

        context "when a revision is not yet in the cache" $ do
          it "adds the revision to the cache" $ do
            let file = cabalFile (path gitCache </> rev)
            touch file
            writeFile file "name: hpack\nversion: 0.1.0"

            populateAddSourceCache_impl gitCache cache (AddSourceDependency name gitDependency)
              `shouldReturn` cachedGitDependency
            doesDirectoryExist (path cachedGitDependencyPath) `shouldReturn` True

        context "when a revision is already in the cache" $ do
          it "does nothing" $ do
            touch (path cachedGitDependencyPath </> ".placeholder")
            populateAddSourceCache_impl gitCache cache (AddSourceDependency name gitDependency)
              `shouldReturn` cachedGitDependency

    context "without subdir" $ do
      let
        subdir = Nothing
        cacheKey = rev
        cabalFile = (</> "hpack.cabal")
      cacheAddSourceDepSpec subdir cacheKey cabalFile

    context "with subdir" $ do
      let
        subdir = Just "subdir"
        cacheKey = show (fingerprintFingerprints [fingerprintString rev, fingerprintString "subdir"])
        cabalFile = (</> "subdir/hpack.cabal")
      cacheAddSourceDepSpec subdir cacheKey cabalFile

  describe "gitClone_impl" $ around_ inTempDirectory $ do
    let
      url = "https://github.com/sol/hpack"
      rev = "6bebd90d1e22901e94460c02bba9d0fa5b343f81"
      cache = "git-cache"
      dst = path cache </> rev
      git = [
          clone
        , stub ("git", ["reset", "--hard", rev], writeFile "rev" rev)
        ]

      clone "git" ["clone", url_, dir] | url_ == url = do
        touch (dir </> ".git" </> ".placeholder")
        writeFile (dir </> "some-source-file") "some-code"

      action inner = do
        mockChain git $ \callProcess_ -> do
          gitClone_impl process {callProcess = callProcess_} cache url (Rev rev) `shouldReturn` CachedRev rev
          inner

    around_ action $ do
      it "clones a git repository" $ do
        readFile (dst </> "some-source-file") `shouldReturn` "some-code"

      it "resets to the specified revision" $ do
        readFile (dst </> "rev") `shouldReturn` rev

      it "removes .git" $ do
        doesDirectoryExist (dst </> ".git") `shouldReturn` False

    context "when revision is already in cache" $ do
      it "does nothing" $ do
        touch (dst </> "some-source-file")
        gitClone_impl process cache url (Rev rev) `shouldReturn` CachedRev rev
        readFile (dst </> "some-source-file") `shouldReturn` ""

  describe "gitRefToRev_impl" $ do
    let
      repo = "http://github.com/sol/with-location"

    it "resolves git references" $ do
      let
        ref = "master"
        output = "517c35a825cbb8eb53fadf4a24654f1227466155        refs/heads/master\n"
        p = process {readProcess = stub ("git", ["ls-remote", repo, ref], "", return output)}
      gitRefToRev_impl p repo (Ref ref) `shouldReturn` "517c35a825cbb8eb53fadf4a24654f1227466155"

    context "when git reference does not exist" $ do
      it "returns an error" $ do
        let
          ref = "master"
          output = ""
          p = process {readProcess = stub ("git", ["ls-remote", repo, ref], "", return output)}
        gitRefToRev_impl p repo (Ref ref) `shouldThrow` errorCall ("invalid reference " ++ show ref ++ " for git repository " ++ repo)

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

  describe "copyPackageConfig" $ (around_ inTempDirectory) $ do
    it "copies package.yaml" $ do
      touch "foo/package.yaml"
      touch "bar/.placeholder"
      copyPackageConfig "foo" "bar"
      doesFileExist "bar/package.yaml" `shouldReturn` True

    context "when package.yaml does not exist" $ do
      it "does nothing" $ do
        touch "foo/.placeholder"
        touch "bar/.placeholder"
        copyPackageConfig "foo" "bar"
        doesFileExist "bar/package.yaml" `shouldReturn` False

  describe "checkCabalName" $ do
    context "when git dependency name and cabal package name match" $ do
      it "succeeds" $ do
        withSystemTempDirectory "tinc" $ \ dir -> do
          let cabalFile = dir </> "foo.cabal"
          writeFile cabalFile "name: foo\nversion: 0.1.0"
          checkCabalName dir (AddSourceDependency "foo" $ Git "<url>" () Nothing)

    context "when git dependency name and cabal package name differ" $ do
      it "fails" $ do
        withSystemTempDirectory "tinc" $ \ dir -> do
          let cabalFile = dir </> "foo.cabal"
          writeFile cabalFile "name: foo\nversion: 0.1.0"
          checkCabalName dir (AddSourceDependency "bar" $ Git "<url>" () Nothing)
            `shouldThrow` errorCall "the git repository <url> contains package \"foo\", expected: \"bar\""

  describe "parseCabalFile" $ do
    it "returns package name and version" $ do
      withSystemTempDirectory "tinc" $ \ dir -> do
        let cabalFile = dir </> "foo.cabal"
        writeFile cabalFile "name: foo\nversion: 0.1.0"
        parseCabalFile dir (Git "<repo>" () Nothing) `shouldReturn` CabalPackage "foo" (makeVersion [0,1,0])

    it "complains about missing version" $ do
      withSystemTempDirectory "tinc" $ \ dir -> do
        let cabalFile = dir </> "foo.cabal"
        writeFile cabalFile "name: foo"
        parseCabalFile dir (Git "<repo>" () Nothing) `shouldThrow` errorCall "the cabal file in git repository <repo> does not specify a version"

    it "complains about invalid cabal files" $ do
      withSystemTempDirectory "tinc" $ \ dir -> do
        let cabalFile = dir </> "foo.cabal"
        writeFile cabalFile "library\n  build-depends: foo bar"
        parseCabalFile dir (Git "<repo>" () Nothing) `shouldThrow` isUserError

  describe "findCabalFile" $ do
    it "finds cabal files in given directory" $ do
      withSystemTempDirectory "tinc" $ \ dir -> do
        let cabalFile = dir </> "foo.cabal"
        touch cabalFile
        findCabalFile dir (Git "<repo>" () Nothing) `shouldReturn` cabalFile

    context "when there is no cabal file" $ do
      it "reports an error" $ do
        withSystemTempDirectory "tinc" $ \ dir -> do
          findCabalFile dir (Git "<repo>" () Nothing) `shouldThrow` errorCall "Couldn't find .cabal file in git repository <repo>"

    context "when there are multiple cabal files" $ do
      it "reports an error" $ do
        withSystemTempDirectory "tinc" $ \ dir -> do
          touch (dir </> "foo.cabal")
          touch (dir </> "bar.cabal")
          findCabalFile dir (Git "<repo>" () Nothing) `shouldThrow` errorCall "Multiple cabal files found in git repository <repo>"

  describe "removeDuplicates" $ do
    let
      foo hash = (AddSource "foo" hash, ())
      foo1 = foo "foo-hash1"
      foo2 = foo "foo-hash2"
      bar = (AddSource "bar" "bar-hash", ())
      baz = (AddSource "baz" "baz-hash", ())

    it "removes duplicates" $ do
      removeDuplicates [Node foo1 [], Node foo2 []] `shouldBe` [foo1]

    context "with a duplicate dependency in a single tree" $ do
      it "uses breadth-first precedence" $ do
        let
          deps = [Node bar [Node foo2 [], Node baz [Node foo1 []]]]
        forAll (permuteForest deps) $ \x -> do
          removeDuplicates x `shouldMatchList` [foo2, bar, baz]

    context "with a duplicate dependency in different trees" $ do
      it "uses breadth-first precedence" $ do
        let
          deps = [
              Node bar [Node foo2 []]
            , Node baz [Node bar [Node foo1 []]]
            ]
        forAll (permuteForest deps) $ \x -> do
          removeDuplicates x `shouldMatchList` [foo2, bar, baz]

permuteTree :: Tree a -> Gen (Tree a)
permuteTree (Node a forest) = Node a <$> permuteForest forest

permuteForest :: Forest a -> Gen (Forest a)
permuteForest xs = mapM permuteTree xs >>= shuffle
