{-# LANGUAGE OverloadedStrings #-}
module Tinc.NixSpec (spec) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Template
import           System.Environment
import           System.FilePath
import           System.IO.Temp
import           Test.Mockery.Environment

import           Helper

import           Tinc.Package
import           Tinc.Nix
import           Tinc.Facts
import           Tinc.Types

withTempHome :: IO () -> IO ()
withTempHome action = withSystemTempDirectory "nix" $ \dir -> do
  env <- filter ((== "PATH") . fst) <$> getEnvironment
  withEnvironment (("HOME", dir) : env) action

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
      nixShell "foo" ["bar", "baz"] `shouldBe` ("nix-shell", ["--run", "foo bar baz"])

    it "escapes arguments" $ do
      nixShell "foo" ["bar baz"] `shouldBe` ("nix-shell", ["--run", "foo 'bar baz'"])

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
              "{ nixpkgs ? import <nixpkgs> {}, compiler ? " ++ show (factsNixResolver facts) ++ " }:"
            , "let"
            , "  oldResolver = builtins.getAttr compiler nixpkgs.haskell.packages;"
            , "  callPackage = oldResolver.callPackage;"
            , ""
            , "  overrideFunction = self: super: rec {"
            , "    foo = callPackage"
            , "      ("
            , "        { mkDerivation, base }:"
            , "        mkDerivation { some derivation; }"
            , "      )"
            , "      { };"
            , "    bar = callPackage"
            , "      ("
            , "        { mkDerivation, base, foo, baz }:"
            , "        mkDerivation { some derivation; }"
            , "      )"
            , "      { inherit foo; inherit (nixpkgs) baz; };"
            , "  };"
            , ""
            , "  newResolver = oldResolver.override {"
            , "    overrides = overrideFunction;"
            , "  };"
            , ""
            , "in newResolver"
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

  describe "templates" $ around_ inTempDirectory $ do
    context "when template files are not present" $ do
      let
        resolver = "ghc7103"
        templateContext = const $ T.pack resolver
        renderTemplate getTemplate = (flip render templateContext) <$> getTemplate

      it "uses default template for shell.nix" $ do
        let
          derivation = TL.pack $ unlines [
              "{ nixpkgs ? import <nixpkgs> {}, compiler ? " ++ show resolver ++ " }:"
            , "(import ./resolver.nix { inherit nixpkgs compiler; }).callPackage ./package.nix { }"
            ]
        renderTemplate defaultNixTemplate `shouldReturn` derivation

      it "uses default template for default.nix" $ do
        let
          derivation = TL.pack $ unlines [
              "{ nixpkgs ? import <nixpkgs> {}, compiler ? " ++ show resolver ++ " }:"
            , "(import ./default.nix { inherit nixpkgs compiler; }).env"
            ]
        renderTemplate shellNixTemplate `shouldReturn` derivation

    context "when templates exist in current directory" $ do
      context "when default.nix.tinc-template exists" $ do
        it "uses default.nix.tinc-template" $ do
          let
            templateFile = "default.nix.tinc-template"
            nixFile = "default.nix"
          writeFile templateFile "some $resolver"
          writeNixFile facts{ factsNixResolver = "foo" } (templateFile, loadTemplateFile templateFile)
          readFile nixFile `shouldReturn` "some foo"

      context "when shell.nix.tinc-template exists" $ do
        it "uses shell.nix.tinc-template" $ do
          let
            templateFile = "shell.nix.tinc-template"
            nixFile = "shell.nix"
          writeFile templateFile "some $resolver"
          writeNixFile facts{ factsNixResolver = "foo" } (templateFile, loadTemplateFile templateFile)
          readFile nixFile `shouldReturn` "some foo"

      context "when template file exists in current directory" $ do
        it "uses current directory template to create nix file" $ do
          let
            templateFile = "foo.nix.tinc-template"
            nixFile = "foo.nix"
          writeFile templateFile "some $resolver"
          writeNixFile facts{ factsNixResolver = "foo" } (templateFile, loadTemplateFile templateFile)
          readFile nixFile `shouldReturn` "some foo"

    context "when template exists in home directory" $ around_ withTempHome $ do
      it "uses template to create nix file" $ do
        let
          templateFile = ".tinc" </> "nix" </> "bar.nix.tinc-template"
          nixFile = "bar.nix"
        touch templateFile
        writeFile templateFile "some $resolver"
        writeNixFile facts{ factsNixResolver = "bar" } (templateFile, loadTemplateFile templateFile)
        readFile nixFile `shouldReturn` "some bar"
