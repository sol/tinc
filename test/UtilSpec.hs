module UtilSpec where

import           System.Directory
import           Test.Hspec
import           Test.Mockery.Directory

import           Util

spec :: Spec
spec = do
  describe "listDirectories" $ do
    it "lists directories" $ do
      inTempDirectory $ do
        createDirectory "foo"
        createDirectory "bar"
        writeFile "baz" ""
        listDirectories "." `shouldReturn` ["./bar", "./foo"]

  describe "listFilesRecursively" $ do
    it "lists files recursively" $ do
      inTempDirectory $ do
        touch "foo"
        touch "bar/baz"
        listFilesRecursively "." >>= (`shouldMatchList` ["./foo", "./bar/baz"])

  describe "fingerprint" $ do
    it "returns a fingerprint for files in specified directory" $ do
      hash1 <- inTempDirectory $ do
        writeFile "foo" "some content"
        touch "bar/baz"
        writeFile "bar/baz" "some other content"
        fingerprint "."

      hash2 <- inTempDirectory $ do
        writeFile "foo" "some content"
        touch "bar/baz"
        writeFile "bar/baz" "some other content"
        fingerprint "."

      hash1 `shouldBe` hash2

    it "takes file contents into account" $ do
      hash1 <- inTempDirectory $ do
        writeFile "foo" "some content"
        fingerprint "."

      hash2 <- inTempDirectory $ do
        writeFile "foo" "some other content"
        fingerprint "."

      hash1 `shouldSatisfy` (/= hash2)

    it "takes filenames into account" $ do
      hash1 <- inTempDirectory $ do
        writeFile "foo" "some content"
        writeFile "bar" "some other content"
        fingerprint "."

      hash2 <- inTempDirectory $ do
        writeFile "bar" "some content"
        writeFile "foo" "some other content"
        fingerprint "."

      hash1 `shouldSatisfy` (/= hash2)

    it "ignores base path of filenames" $ do
      hash1 <- inTempDirectory $ do
        touch "foo/bar"
        writeFile "foo/bar" "some content"
        fingerprint "foo"

      hash2 <- inTempDirectory $ do
        touch "baz/bar"
        writeFile "baz/bar" "some content"
        fingerprint "baz"

      hash1 `shouldBe` hash2

  describe "cachedIO" $ do
    it "runs given action" $ do
      inTempDirectory $ do
        cachedIO "foo" (return "bar") `shouldReturn` "bar"

    it "caches the result of the given action" $ do
      inTempDirectory $ do
        _ <- cachedIO "foo" (return "bar")
        readFile "foo" `shouldReturn` "bar"

    it "reuses cached result" $ do
      inTempDirectory $ do
        writeFile "foo" "bar"
        cachedIO "foo" undefined `shouldReturn` "bar"

  describe "getCabalFiles" $ around_ inTempDirectory $ do
    it "returns all cabal files in the current directory" $ do
      touch "foo.cabal"
      touch "bar.cabal"
      getCabalFiles >>= (`shouldMatchList` ["bar.cabal", "foo.cabal"])

    it "ignores dot files" $ do
      touch ".foo.cabal"
      getCabalFiles `shouldReturn` []
