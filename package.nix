{ mkDerivation, aeson, base, bytestring, Cabal, call-stack
, containers, directory, exceptions, filelock, filepath, gitrev
, graph-wrapper, hpack, hspec, HUnit, language-dot, mockery, parsec
, process, QuickCheck, safe, stdenv, store, temporary, time
, transformers, unix, unix-compat, yaml
}:
mkDerivation {
  pname = "tinc";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring Cabal call-stack containers directory
    exceptions filelock filepath gitrev graph-wrapper hpack
    language-dot parsec process store temporary time transformers
    unix-compat yaml
  ];
  testHaskellDepends = [
    aeson base bytestring Cabal call-stack containers directory
    exceptions filelock filepath gitrev graph-wrapper hpack hspec HUnit
    language-dot mockery parsec process QuickCheck safe store temporary
    time transformers unix unix-compat yaml
  ];
  homepage = "https://github.com/sol/tinc#readme";
  license = stdenv.lib.licenses.mit;
}
