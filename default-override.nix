let
  tinc = import ./tinc.nix;
in { nixpkgs ? import <nixpkgs> {}, compiler ? tinc.compiler }:
  let
    resolver = tinc.resolver { inherit nixpkgs compiler; };
    oldDrv = resolver.callPackage ./package.nix {};
    makeWrapper = nixpkgs.makeWrapper;
    ghcPackages = nixpkgs.haskell.packages.ghc7103;
    ghc = ghcPackages.ghc;
    cabal2nix = ghcPackages.cabal2nix;
    cabal-install = ghcPackages.cabal-install;
  in nixpkgs.lib.overrideDerivation oldDrv (oldAttrs: {
    doCheck = false;
    configureFlags = [ "--disable-tests" ];
    postInstall = ''
      source ${makeWrapper}/nix-support/setup-hook
      wrapProgram $out/bin/tinc \
        --prefix PATH : '${ghc}/bin' \
        --prefix PATH : '${cabal2nix}/bin' \
        --prefix PATH : '${cabal-install}/bin'
    '';
  })
