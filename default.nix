{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7103" }:
(import ./resolver.nix { inherit nixpkgs compiler; }).callPackage ./package.nix { }
