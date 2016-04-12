let
  tinc = import ./tinc.nix;
in
{ nixpkgs ? import <nixpkgs> {}, compiler ? tinc.compiler }:
(import ./default.nix { inherit nixpkgs compiler; }).env
