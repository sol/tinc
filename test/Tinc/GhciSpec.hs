module Tinc.GhciSpec (spec) where

import           Test.Hspec
import           Test.Mockery.Directory

import           Tinc.Ghci

spec :: Spec
spec = around_ inTempDirectory $ do
  describe "ghciSpec" $ do
    it "returns a command specification for ghci" $ do
      ghciSpec ["test/Spec.hs"] [] `shouldReturn` ("cabal", ["exec", "--", "ghci", "-idist/build/", "-idist/build/autogen", "test/Spec.hs"])

    context "when cabal_macros.h exists" $ do
      it "includes cabal_macros.h" $ do
        touch "dist/build/autogen/cabal_macros.h"
        ghciSpec [] [] `shouldReturn` ("cabal", ["exec", "--", "ghci", "-idist/build/", "-idist/build/autogen", "-optP-includedist/build/autogen/cabal_macros.h"])

    context "with include dirs" $ do
      it "prefixes them with -I" $ do
        ghciSpec [] ["some/include/dir"] `shouldReturn` ("cabal", ["exec", "--", "ghci", "-idist/build/", "-idist/build/autogen", "-Isome/include/dir"])

  describe "extractIncludeDirs" $ do
    it "extracts include-dirs from ghc-pkg dump output" $ do
      let dumpOutput = unlines [
              "foo"
            , "include-dirs: /some/include/dir"
            , "bar"
            ]
      extractIncludeDirs dumpOutput `shouldBe` ["/some/include/dir"]
   
