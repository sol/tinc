{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7103" }:
(import ./tinc.nix { inherit nixpkgs compiler; }).callPackage ./package.nix { }
