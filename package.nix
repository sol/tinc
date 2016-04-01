{ callPackage, mkDerivation, base, bytestring, Cabal, containers, directory, filepath, process, stdenv, time, transformers, unix }:
let
  pkgs = (import <nixpkgs> {}).pkgs;
  HUnit = callPackage /home/kint/.tinc/cache/nix/HUnit-1.3.1.1.nix { };
  ansi-terminal = callPackage /home/kint/.tinc/cache/nix/ansi-terminal-0.6.2.3.nix { };
  base-compat = callPackage /home/kint/.tinc/cache/nix/base-compat-0.9.0.nix { inherit hspec QuickCheck; };
  dlist = callPackage /home/kint/.tinc/cache/nix/dlist-0.7.1.2.nix { inherit QuickCheck; };
  Glob = callPackage /home/kint/.tinc/cache/nix/Glob-0.7.5.nix { inherit dlist; };
  fail = callPackage /home/kint/.tinc/cache/nix/fail-4.9.0.0.nix { };
  filelock = callPackage /home/kint/.tinc/cache/nix/filelock-0.1.0.1.nix { };
  gitrev = callPackage /home/kint/.tinc/cache/nix/gitrev-1.2.0.nix { };
  graph-wrapper = callPackage /home/kint/.tinc/cache/nix/graph-wrapper-0.2.5.1.nix { inherit hspec QuickCheck; };
  hspec-discover = callPackage /home/kint/.tinc/cache/nix/hspec-discover-2.2.3.nix { };
  hspec-expectations = callPackage /home/kint/.tinc/cache/nix/hspec-expectations-0.7.2.nix { inherit HUnit; };
  logging-facade = callPackage /home/kint/.tinc/cache/nix/logging-facade-0.1.1.nix { inherit hspec; };
  mtl = callPackage /home/kint/.tinc/cache/nix/mtl-2.2.1.nix { };
  primitive = callPackage /home/kint/.tinc/cache/nix/primitive-0.6.1.0.nix { };
  random = callPackage /home/kint/.tinc/cache/nix/random-1.1.nix { };
  safe = callPackage /home/kint/.tinc/cache/nix/safe-0.3.9.nix { };
  setenv = callPackage /home/kint/.tinc/cache/nix/setenv-0.1.1.3.nix { };
  stm = callPackage /home/kint/.tinc/cache/nix/stm-2.4.4.1.nix { };
  async = callPackage /home/kint/.tinc/cache/nix/async-2.1.0.nix { inherit HUnit stm; };
  syb = callPackage /home/kint/.tinc/cache/nix/syb-0.6.nix { inherit HUnit mtl; };
  tagged = callPackage /home/kint/.tinc/cache/nix/tagged-0.8.3.nix { };
  text = callPackage /home/kint/.tinc/cache/nix/text-1.2.2.1.nix { inherit HUnit QuickCheck random; };
  hashable = callPackage /home/kint/.tinc/cache/nix/hashable-1.2.4.0.nix { inherit HUnit QuickCheck random text; };
  parsec = callPackage /home/kint/.tinc/cache/nix/parsec-3.1.9.nix { inherit HUnit mtl text; };
  language-dot = callPackage /home/kint/.tinc/cache/nix/language-dot-0.0.8.nix { inherit mtl parsec; };
  tf-random = callPackage /home/kint/.tinc/cache/nix/tf-random-0.5.nix { inherit primitive random; };
  QuickCheck = callPackage /home/kint/.tinc/cache/nix/QuickCheck-2.8.2.nix { inherit random tf-random; };
  quickcheck-io = callPackage /home/kint/.tinc/cache/nix/quickcheck-io-0.1.2.nix { inherit HUnit QuickCheck; };
  hspec-core = callPackage /home/kint/.tinc/cache/nix/hspec-core-2.2.3.nix { inherit ansi-terminal async hspec-expectations HUnit QuickCheck quickcheck-io random setenv tf-random; };
  hspec = callPackage /home/kint/.tinc/cache/nix/hspec-2.2.3.nix { inherit hspec-core hspec-discover hspec-expectations HUnit QuickCheck; };
  transformers-compat = callPackage /home/kint/.tinc/cache/nix/transformers-compat-0.5.1.4.nix { };
  exceptions = callPackage /home/kint/.tinc/cache/nix/exceptions-0.8.2.1.nix { inherit mtl QuickCheck stm transformers-compat; };
  mmorph = callPackage /home/kint/.tinc/cache/nix/mmorph-1.0.6.nix { inherit mtl transformers-compat; };
  temporary = callPackage /home/kint/.tinc/cache/nix/temporary-1.2.0.4.nix { inherit exceptions; };
  mockery = callPackage /home/kint/.tinc/cache/nix/mockery-0.3.3.nix { inherit base-compat hspec logging-facade temporary; };
  transformers-base = callPackage /home/kint/.tinc/cache/nix/transformers-base-0.4.4.nix { inherit stm transformers-compat; };
  monad-control = callPackage /home/kint/.tinc/cache/nix/monad-control-1.0.0.5.nix { inherit stm transformers-base transformers-compat; };
  lifted-base = callPackage /home/kint/.tinc/cache/nix/lifted-base-0.2.3.6.nix { inherit HUnit monad-control transformers-base transformers-compat; };
  enclosed-exceptions = callPackage /home/kint/.tinc/cache/nix/enclosed-exceptions-1.0.1.1.nix { inherit async hspec lifted-base monad-control QuickCheck transformers-base; };
  resourcet = callPackage /home/kint/.tinc/cache/nix/resourcet-1.1.7.3.nix { inherit exceptions hspec lifted-base mmorph monad-control mtl transformers-base transformers-compat; };
  conduit = callPackage /home/kint/.tinc/cache/nix/conduit-1.2.6.4.nix { inherit exceptions hspec lifted-base mmorph mtl QuickCheck resourcet safe transformers-base; };
  unix-compat = callPackage /home/kint/.tinc/cache/nix/unix-compat-0.4.1.4.nix { };
  unordered-containers = callPackage /home/kint/.tinc/cache/nix/unordered-containers-0.2.7.0.nix { inherit hashable HUnit QuickCheck; };
  semigroups = callPackage /home/kint/.tinc/cache/nix/semigroups-0.18.1.nix { inherit hashable tagged text unordered-containers; };
  vector = callPackage /home/kint/.tinc/cache/nix/vector-0.11.0.0.nix { inherit primitive QuickCheck random; };
  scientific = callPackage /home/kint/.tinc/cache/nix/scientific-0.3.4.6.nix { inherit hashable QuickCheck text vector; };
  attoparsec = callPackage /home/kint/.tinc/cache/nix/attoparsec-0.13.0.1.nix { inherit QuickCheck scientific text vector; };
  aeson = callPackage /home/kint/.tinc/cache/nix/aeson-0.11.1.4.nix { inherit attoparsec dlist fail hashable HUnit mtl QuickCheck scientific semigroups syb tagged text unordered-containers vector; };
  with-location = callPackage /home/kint/.tinc/cache/nix/with-location-0.1.0.nix { inherit hspec; };
  yaml = callPackage /home/kint/.tinc/cache/nix/yaml-0.8.16.nix { inherit aeson attoparsec base-compat conduit enclosed-exceptions hspec HUnit mockery resourcet scientific semigroups text unordered-containers vector; };
  hpack = callPackage /home/kint/.tinc/cache/nix/hpack-0.11.2.nix { inherit aeson base-compat Glob hspec mockery temporary text unordered-containers yaml; };
in 
mkDerivation {
  pname = "tinc";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base-compat bytestring Cabal containers directory
    exceptions filelock filepath gitrev graph-wrapper hpack
    language-dot parsec process temporary time transformers unix-compat
    with-location yaml
  ];
  testHaskellDepends = [
    aeson base base-compat bytestring Cabal containers directory
    exceptions filelock filepath gitrev graph-wrapper hpack hspec HUnit
    language-dot mockery parsec process safe temporary time
    transformers unix unix-compat with-location yaml
  ];
  homepage = "https://github.com/sol/tinc#readme";
  license = stdenv.lib.licenses.mit;
}

