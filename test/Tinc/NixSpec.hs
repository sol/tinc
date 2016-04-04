{-# LANGUAGE OverloadedStrings #-}
module Tinc.NixSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Helper

import           Tinc.Package
import           Tinc.Nix


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
      pkgImport cache (Package "HUnit" "1.3.1.1", [], []) `shouldBe` "HUnit = callPackage /path/to/nix/cache/HUnit-1.3.1.1.nix { };"

    context "when given a list of Haskell dependencies" $ do
      it "specifies the dependencies in the expression" $ do
        pkgImport cache (Package "foo" "0.1.0", ["bar", "baz"], []) `shouldBe` "foo = callPackage /path/to/nix/cache/foo-0.1.0.nix { inherit bar baz; };"

    context "when given a list of system dependencies" $ do
      it "specifies the dependencies in the expression" $ do
        pkgImport cache (Package "foo" "0.1.0", [], ["bar"]) `shouldBe` "foo = callPackage /path/to/nix/cache/foo-0.1.0.nix { inherit (pkgs) bar; };"

  describe "defaultDerivation" $ do
    it "generates default derivation" $ do
      defaultDerivation facts `shouldBe` unlines [
          "{ nixpkgs ? import <nixpkgs> {}, compiler ? \"ghc7103\" }:"
        , "nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./package.nix { }"
        ]

  describe "shellDerivation" $ do
    it "generates shell derivation" $ do
      shellDerivation facts `shouldBe` unlines [
          "{ nixpkgs ? import <nixpkgs> {}, compiler ? \"ghc7103\" }:"
        , "(import ./default.nix { inherit nixpkgs compiler; }).env"
        ]

  describe "projectDerivation" $ do
    it "generates project derivation" $ do
      let dependencies = [
              (Package "foo" "0.1.0", [], [])
            , (Package "bar" "0.1.0", ["foo"], ["baz"])
            ]
          pkgDerivation = "{ mkDerivation, base, foo }: { someDerivation }"
      projectDerivation cache pkgDerivation dependencies `shouldBe` unlines [
          "{ callPackage, mkDerivation, base }:"
        , "let"
        , "  pkgs = (import <nixpkgs> {}).pkgs;"
        , "  foo = callPackage /path/to/nix/cache/foo-0.1.0.nix { };"
        , "  bar = callPackage /path/to/nix/cache/bar-0.1.0.nix { inherit foo; inherit (pkgs) baz; };"
        , "in { someDerivation }"
        ]

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
