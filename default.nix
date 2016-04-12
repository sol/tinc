let
  tinc = import ./tinc.nix;
  default ={ nixpkgs ? import <nixpkgs> {}, compiler ? tinc.compiler }:
    (tinc.resolver { inherit nixpkgs compiler; }).callPackage ./package.nix {};
  overrideFile = ./default-override.nix;
  expr = if builtins.pathExists overrideFile then import overrideFile else default;
in expr
