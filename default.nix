let
  default = { nixpkgs ? import <nixpkgs> {} }:
    (import ./tinc.nix { inherit nixpkgs; }).resolver.callPackage ./package.nix {};
  overrideFile = ./default-override.nix;
  expr = if builtins.pathExists overrideFile then import overrideFile else default;
in expr
