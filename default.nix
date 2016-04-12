let
  tinc = import ./tinc.nix;
in
{ nixpkgs ? import <nixpkgs> {}, compiler ? tinc.compiler }:
(tinc.resolver { inherit nixpkgs compiler; }).callPackage ./package.nix {}
