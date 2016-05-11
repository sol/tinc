{-# LANGUAGE OverloadedStrings #-}
module Tinc.NixSpec (spec) where

import           Helper
import           System.FilePath
import           System.IO.Temp

import           Tinc.Package
import           Tinc.Nix
import           Tinc.Facts
import           Tinc.Types


spec :: Spec
spec = do
  let cache = "/path/to/nix/cache"

  describe "cabal" $ do
    it "executes cabal in an empty ghc environment" $ do
      cabal facts ["sandbox", "init"] `shouldBe` ("nix-shell", ["-p", "haskell.packages.\"ghc7103\".ghcWithPackages (p: [ p.cabal-install ])", "--pure", "--run", "cabal sandbox init"])

    it "escapes arguments" $ do
      cabal facts ["sandbox init"] `shouldBe` ("nix-shell", ["-p", "haskell.packages.\"ghc7103\".ghcWithPackages (p: [ p.cabal-install ])", "--pure", "--run", "cabal 'sandbox init'"])

  describe "nixShell" $ do
    it "executes command in project environment" $ do
      nixShell "foo" ["bar", "baz"] `shouldBe` ("nix-shell", ["shell.nix", "--run", "foo bar baz"])

    it "escapes arguments" $ do
      nixShell "foo" ["bar baz"] `shouldBe` ("nix-shell", ["shell.nix", "--run", "foo 'bar baz'"])

  describe "pkgImport" $ do
    it "imports a package" $ do
      let
        derivation = unlines [
            "{ mkDerivation }:"
          , "mkDerivation { some derivation; }"
          ]
        inlined = [
            "foo = callPackage"
          , "  ("
          , "    { mkDerivation }:"
          , "    mkDerivation { some derivation; }"
          , "  )"
          , "  { };"
          ]
      pkgImport (Package "foo" "0.1.0", [], []) derivation `shouldBe` inlined;

    context "when given a list of Haskell dependencies" $ do
      it "specifies the dependencies in the expression" $ do
        let
          derivation = unlines [
              "{ mkDerivation, bar, baz }:"
            , "mkDerivation { some derivation; }"
            ]
          inlined = [
              "foo = callPackage"
            , "  ("
            , "    { mkDerivation, bar, baz }:"
            , "    mkDerivation { some derivation; }"
            , "  )"
            , "  { inherit bar baz; };"
            ]
        pkgImport (Package "foo" "0.1.0", ["bar", "baz"], []) derivation `shouldBe` inlined

    context "when given a list of system dependencies" $ do
      it "specifies the dependencies in the expression" $ do
        let
          derivation = unlines [
              "{ mkDerivation, bar }:"
            , "mkDerivation { some derivation; }"
            ]
          inlined = [
              "foo = callPackage"
            , "  ("
            , "    { mkDerivation, bar }:"
            , "    mkDerivation { some derivation; }"
            , "  )"
            , "  { inherit (nixpkgs) bar; };"
            ]
        pkgImport (Package "foo" "0.1.0", [], ["bar"]) derivation `shouldBe` inlined;

  describe "resolverDerivation" $ do
    it "generates resolver derivation" $ do
      let dependencies = [
              (Package "foo" "0.1.0", [], [])
            , (Package "bar" "0.1.0", ["foo"], ["baz"])
            ]
          fooDerivation = unlines [
              "{ mkDerivation, base }:"
            , "mkDerivation { some derivation; }"
            ]
          barDerivation = unlines [
              "{ mkDerivation, base, foo, baz }:"
            , "mkDerivation { some derivation; }"
            ]
          resolver = unlines [
              "rec {"
            , "  compiler = nixpkgs.haskell.packages.\"ghc7103\";"
            , "  resolver = { nixpkgs, compiler }:"
            , "    let"
            , "      callPackage = compiler.callPackage;"
            , ""
            , "      overrideFunction = self: super: rec {"
            , "        foo = callPackage"
            , "          ("
            , "            { mkDerivation, base }:"
            , "            mkDerivation { some derivation; }"
            , "          )"
            , "          { };"
            , "        bar = callPackage"
            , "          ("
            , "            { mkDerivation, base, foo, baz }:"
            , "            mkDerivation { some derivation; }"
            , "          )"
            , "          { inherit foo; inherit (nixpkgs) baz; };"
            , "      };"
            , ""
            , "      newResolver = compiler.override {"
            , "        overrides = overrideFunction;"
            , "      };"
            , ""
            , "    in newResolver;"
            , "}"
            ]

      withSystemTempDirectory "tinc" $ \dir -> do
        writeFile (dir </> "foo-0.1.0.nix") fooDerivation
        writeFile (dir </> "bar-0.1.0.nix") barDerivation
        resolverDerivation facts{ factsNixCache = Path dir } dependencies `shouldReturn` resolver

  describe "parseNixFunction" $ do
    it "parses a Nix function" $ do
      let nixFunction = "{ mkDerivation, aeson }: mkDerivation { someDerivation }"
      parseNixFunction nixFunction `shouldBe` Function ["mkDerivation", "aeson"] "mkDerivation { someDerivation }"

  describe "disableTests" $ do
    let derivation = unlines [
                  "{ mkDerivation }:"
                , "mkDerivation {"
                , "  ..."
                , "}"
                ]
        derivationWithoutTests = unlines [
                  "{ mkDerivation }:"
                , "mkDerivation {"
                , "  ..."
                , "  doCheck = false;"
                , "}"
                ]

    it "disables tests" $ do
      disableTests derivation `shouldBe` derivationWithoutTests

    context "when tests are already disabled" $ do
      it "does nothing" $ do
        disableTests derivationWithoutTests `shouldBe` derivationWithoutTests

  describe "extractDependencies" $ do
    let knownHaskellDependencies = ["hspec", "aeson", "zlib"]
    it "extract Haskell dependencies" $ do
      let function = Function ["mkDerivation", "hspec"] ""
      extractDependencies function knownHaskellDependencies `shouldBe` (["hspec"], [])

    it "extract system dependencies" $ do
      let function = Function ["mkDerivation", "foo"] "  librarySystemDepends = [ foo ];"
      extractDependencies function knownHaskellDependencies `shouldBe` ([], ["foo"])

    context "when a known Haskell dependency has the same name as a system dependency" $ do
      it "omits the Haskell dependency with that name" $ do
        let function = Function ["mkDerivation", "zlib"] "  librarySystemDepends = [ zlib ];"
        extractDependencies function knownHaskellDependencies `shouldBe` ([], ["zlib"])

  describe "derivationFile" $ do
    it "returns path to derivation file" $ do
      derivationFile cache (Package "foo" "0.1.0") `shouldBe` "/path/to/nix/cache/foo-0.1.0.nix"

    context "when package has a git revision" $ do
      it "includes the git revision in the filename" $ do
        let package = Package "foo" (Version "0.1.0" $ Just "some-git-rev")
        derivationFile cache package `shouldBe` "/path/to/nix/cache/foo-0.1.0-some-git-rev.nix"
