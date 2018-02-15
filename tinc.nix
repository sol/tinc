{ nixpkgs }:
rec {
  compiler = nixpkgs.haskellPackages;
  resolver =
    let
      callPackage = compiler.callPackage;

      overrideFunction = self: super: rec {
        base-compat = callPackage
          (
            { mkDerivation, base, hspec, QuickCheck, stdenv, unix }:
            mkDerivation {
              pname = "base-compat";
              version = "0.9.3";
              sha256 = "7d602b0f0543fadbd598a090c738e9ce9b07a1896673dc27f1503ae3bea1a210";
              libraryHaskellDepends = [ base unix ];
              testHaskellDepends = [ base hspec QuickCheck ];
              description = "A compatibility layer for base";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hspec QuickCheck; };
        base-orphans = callPackage
          (
            { mkDerivation, base, ghc-prim, hspec, QuickCheck, stdenv }:
            mkDerivation {
              pname = "base-orphans";
              version = "0.6";
              sha256 = "c7282aa7516652e6e4a78ccdfb654a99c9da683875748ad5898a3f200be7ad0e";
              libraryHaskellDepends = [ base ghc-prim ];
              testHaskellDepends = [ base hspec QuickCheck ];
              homepage = "https://github.com/haskell-compat/base-orphans#readme";
              description = "Backwards-compatible orphan instances for base";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hspec QuickCheck; };
        base64-bytestring = callPackage
          (
            { mkDerivation, base, bytestring, containers, HUnit, QuickCheck
            , stdenv, test-framework, test-framework-hunit
            , test-framework-quickcheck2
            }:
            mkDerivation {
              pname = "base64-bytestring";
              version = "1.0.0.1";
              sha256 = "ab25abf4b00a2f52b270bc3ed43f1d59f16c8eec9d7dffb14df1e9265b233b50";
              libraryHaskellDepends = [ base bytestring ];
              testHaskellDepends = [
                base bytestring containers HUnit QuickCheck test-framework
                test-framework-hunit test-framework-quickcheck2
              ];
              homepage = "https://github.com/bos/base64-bytestring";
              description = "Fast base64 encoding and decoding for ByteStrings";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit QuickCheck; };
        basement = callPackage
          (
            { mkDerivation, base, ghc-prim, stdenv }:
            mkDerivation {
              pname = "basement";
              version = "0.0.6";
              sha256 = "9ca23b940006d8c6a7bc9c07c4ef1bf5ddb47ce82d384c5f341996e22cb95ff7";
              libraryHaskellDepends = [ base ghc-prim ];
              homepage = "https://github.com/haskell-foundation/foundation";
              description = "Foundation scrap box of array & string";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        byteable = callPackage
          (
            { mkDerivation, base, bytestring, stdenv }:
            mkDerivation {
              pname = "byteable";
              version = "0.1.1";
              sha256 = "243b34a1b5b64b39e39fe58f75c18f6cad5b668b10cabcd86816cbde27783fe2";
              enableSeparateDataOutput = true;
              libraryHaskellDepends = [ base bytestring ];
              homepage = "http://github.com/vincenthz/hs-byteable";
              description = "Type class for sequence of bytes";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        cabal-doctest = callPackage
          (
            { mkDerivation, base, Cabal, directory, filepath, stdenv }:
            mkDerivation {
              pname = "cabal-doctest";
              version = "1.0.6";
              sha256 = "decaaa5a73eaabaf3c4f8c644bd7f6e3f428b6244e935c0cf105f75f9b24ed2d";
              libraryHaskellDepends = [ base Cabal directory filepath ];
              homepage = "https://github.com/phadej/cabal-doctest";
              description = "A Setup.hs helper for doctests running";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        call-stack = callPackage
          (
            { mkDerivation, base, nanospec, stdenv }:
            mkDerivation {
              pname = "call-stack";
              version = "0.1.0";
              sha256 = "f25f5e0992a39371079cc25c2a14b5abb872fa7d868a32753aac3a258b83b1e2";
              libraryHaskellDepends = [ base ];
              testHaskellDepends = [ base nanospec ];
              homepage = "https://github.com/sol/call-stack#readme";
              description = "Use GHC call-stacks in a backward compatible way";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        colour = callPackage
          (
            { mkDerivation, base, QuickCheck, random, stdenv, test-framework
            , test-framework-quickcheck2
            }:
            mkDerivation {
              pname = "colour";
              version = "2.3.4";
              sha256 = "0f439f00b322ce3d551f28a4dd1520aa2c91d699de4cdc6d485b9b04be0dc5eb";
              enableSeparateDataOutput = true;
              libraryHaskellDepends = [ base ];
              testHaskellDepends = [
                base QuickCheck random test-framework test-framework-quickcheck2
              ];
              homepage = "http://www.haskell.org/haskellwiki/Colour";
              description = "A model for human colour/color perception";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit QuickCheck random; };
        dlist = callPackage
          (
            { mkDerivation, base, Cabal, deepseq, QuickCheck, stdenv }:
            mkDerivation {
              pname = "dlist";
              version = "0.8.0.4";
              sha256 = "acf1867b80cdd618b8d904e89eea33be60d3c4c3aeb80d61f29229a301cc397a";
              libraryHaskellDepends = [ base deepseq ];
              testHaskellDepends = [ base Cabal QuickCheck ];
              homepage = "https://github.com/spl/dlist";
              description = "Difference lists";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit QuickCheck; };
        fail = callPackage
          (
            { mkDerivation, stdenv }:
            mkDerivation {
              pname = "fail";
              version = "4.9.0.0";
              sha256 = "6d5cdb1a5c539425a9665f740e364722e1d9d6ae37fbc55f30fe3dbbbb91d4a2";
              homepage = "https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail";
              description = "Forward-compatible MonadFail class";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        filelock = callPackage
          (
            { mkDerivation, async, base, process, stdenv, unix }:
            mkDerivation {
              pname = "filelock";
              version = "0.1.1.2";
              sha256 = "0ff1dcb13ec619f72496035e2a1298ef9dc6a814ba304d882cd9b145eae3203d";
              libraryHaskellDepends = [ base unix ];
              testHaskellDepends = [ async base process ];
              homepage = "http://github.com/takano-akio/filelock";
              description = "Portable interface to file locking (flock / LockFileEx)";
              license = stdenv.lib.licenses.publicDomain;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit async; };
        graph-wrapper = callPackage
          (
            { mkDerivation, array, base, containers, deepseq, hspec, QuickCheck
            , stdenv
            }:
            mkDerivation {
              pname = "graph-wrapper";
              version = "0.2.5.1";
              sha256 = "8361853fca2d2251bd233e18393053dd391d21ca6f210b2bc861b0e0f4c2e113";
              libraryHaskellDepends = [ array base containers ];
              testHaskellDepends = [
                array base containers deepseq hspec QuickCheck
              ];
              homepage = "https://github.com/soenkehahn/graph-wrapper";
              description = "A wrapper around the standard Data.Graph with a less awkward interface";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hspec QuickCheck; };
        hspec-discover = callPackage
          (
            { mkDerivation, base, directory, filepath, hspec-meta, QuickCheck
            , stdenv
            }:
            mkDerivation {
              pname = "hspec-discover";
              version = "2.4.8";
              sha256 = "6ad28a1f1ae52f71fa9e5c1188abfd58d5b41a75802b86723bf1ba27af6b9c52";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [ base directory filepath ];
              executableHaskellDepends = [ base directory filepath ];
              testHaskellDepends = [
                base directory filepath hspec-meta QuickCheck
              ];
              homepage = "http://hspec.github.io/";
              description = "Automatically discover and run Hspec tests";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit QuickCheck; };
        integer-logarithms = callPackage
          (
            { mkDerivation, array, base, ghc-prim, integer-gmp, QuickCheck
            , smallcheck, stdenv, tasty, tasty-hunit, tasty-quickcheck
            , tasty-smallcheck
            }:
            mkDerivation {
              pname = "integer-logarithms";
              version = "1.0.2";
              sha256 = "31069ccbff489baf6c4a93cb7475640aabea9366eb0b583236f10714a682b570";
              revision = "1";
              editedCabalFile = "0sccd0d6qrcm3a7nni5lqv40g5m5knf965z4skkgbyyhb3z6qsq8";
              libraryHaskellDepends = [ array base ghc-prim integer-gmp ];
              testHaskellDepends = [
                base QuickCheck smallcheck tasty tasty-hunit tasty-quickcheck
                tasty-smallcheck
              ];
              homepage = "https://github.com/phadej/integer-logarithms";
              description = "Integer logarithms";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit QuickCheck smallcheck; };
        mtl = callPackage
          (
            { mkDerivation, base, stdenv, transformers }:
            mkDerivation {
              pname = "mtl";
              version = "2.2.1";
              sha256 = "cae59d79f3a16f8e9f3c9adc1010c7c6cdddc73e8a97ff4305f6439d855c8dc5";
              revision = "1";
              editedCabalFile = "0fsa965g9h23mlfjzghmmhcb9dmaq8zpm374gby6iwgdx47q0njb";
              libraryHaskellDepends = [ base transformers ];
              homepage = "http://github.com/ekmett/mtl";
              description = "Monad classes, using functional dependencies";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        network = callPackage
          (
            { mkDerivation, base, bytestring, doctest, HUnit, stdenv
            , test-framework, test-framework-hunit, unix
            }:
            mkDerivation {
              pname = "network";
              version = "2.6.3.3";
              sha256 = "776668b0a969d0d57ebabf78943cfc21a1aaf7e5e2ae6288322292125c9440f5";
              revision = "1";
              editedCabalFile = "0nh9sbbyj3jdm2ybffsxa3c4mdywy3wq48sg8d5ylkr2s6cmbbpz";
              libraryHaskellDepends = [ base bytestring unix ];
              testHaskellDepends = [
                base bytestring doctest HUnit test-framework test-framework-hunit
              ];
              homepage = "https://github.com/haskell/network";
              description = "Low-level networking interface";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit; };
        primitive = callPackage
          (
            { mkDerivation, base, ghc-prim, stdenv, transformers }:
            mkDerivation {
              pname = "primitive";
              version = "0.6.3.0";
              sha256 = "cddeff804e0f577f1be0179d5d145dfc170f8bfb66f663b9fba67104a45d9555";
              libraryHaskellDepends = [ base ghc-prim transformers ];
              testHaskellDepends = [ base ghc-prim ];
              homepage = "https://github.com/haskell/primitive";
              description = "Primitive memory-related operations";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        random = callPackage
          (
            { mkDerivation, base, stdenv, time }:
            mkDerivation {
              pname = "random";
              version = "1.1";
              sha256 = "b718a41057e25a3a71df693ab0fe2263d492e759679b3c2fea6ea33b171d3a5a";
              revision = "1";
              editedCabalFile = "1pv5d7bm2rgap7llp5vjsplrg048gvf0226y0v19gpvdsx7n4rvv";
              libraryHaskellDepends = [ base time ];
              testHaskellDepends = [ base ];
              description = "random number library";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        safe = callPackage
          (
            { mkDerivation, base, deepseq, QuickCheck, stdenv }:
            mkDerivation {
              pname = "safe";
              version = "0.3.16";
              sha256 = "688ae558289256aeddd8f70ca4303c36de0bb37cb70b1094a0fd4731e0235975";
              libraryHaskellDepends = [ base ];
              testHaskellDepends = [ base deepseq QuickCheck ];
              homepage = "https://github.com/ndmitchell/safe#readme";
              description = "Library of safe (exception free) functions";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit QuickCheck; };
        semigroups = callPackage
          (
            { mkDerivation, base, stdenv }:
            mkDerivation {
              pname = "semigroups";
              version = "0.18.4";
              sha256 = "589e3042329a6bcffb5c0e85834143586db22eb7a2aae094d492cd004f685d27";
              libraryHaskellDepends = [ base ];
              homepage = "http://github.com/ekmett/semigroups/";
              description = "Anything that associates";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        setenv = callPackage
          (
            { mkDerivation, base, stdenv, unix }:
            mkDerivation {
              pname = "setenv";
              version = "0.1.1.3";
              sha256 = "e358df39afc03d5a39e2ec650652d845c85c80cc98fe331654deafb4767ecb32";
              revision = "1";
              editedCabalFile = "0ny4g3kjys0hqg41mnwrsymy1bwhl8l169kis4y4fa58sb06m4f5";
              libraryHaskellDepends = [ base unix ];
              description = "A cross-platform library for setting environment variables";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        split = callPackage
          (
            { mkDerivation, base, QuickCheck, stdenv }:
            mkDerivation {
              pname = "split";
              version = "0.2.3.3";
              sha256 = "1dcd674f7c5f276f33300f5fd59e49d1ac6fc92ae949fd06a0f6d3e9d9ac1413";
              libraryHaskellDepends = [ base ];
              testHaskellDepends = [ base QuickCheck ];
              description = "Combinator library for splitting lists";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit QuickCheck; };
        stm = callPackage
          (
            { mkDerivation, array, base, stdenv }:
            mkDerivation {
              pname = "stm";
              version = "2.4.5.0";
              sha256 = "31d7db183f13beed5c71409d12747a7f4cf3e145630553dc86336208540859a7";
              libraryHaskellDepends = [ array base ];
              homepage = "https://wiki.haskell.org/Software_transactional_memory";
              description = "Software Transactional Memory";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        syb = callPackage
          (
            { mkDerivation, base, containers, HUnit, mtl, stdenv }:
            mkDerivation {
              pname = "syb";
              version = "0.7";
              sha256 = "b8757dce5ab4045c49a0ae90407d575b87ee5523a7dd5dfa5c9d54fcceff42b5";
              libraryHaskellDepends = [ base ];
              testHaskellDepends = [ base containers HUnit mtl ];
              homepage = "http://www.cs.uu.nl/wiki/GenericProgramming/SYB";
              description = "Scrap Your Boilerplate";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit mtl; };
        text = callPackage
          (
            { mkDerivation, array, base, binary, bytestring, deepseq, directory
            , ghc-prim, HUnit, integer-gmp, QuickCheck, quickcheck-unicode
            , random, stdenv, test-framework, test-framework-hunit
            , test-framework-quickcheck2
            }:
            mkDerivation {
              pname = "text";
              version = "1.2.3.0";
              sha256 = "20e0b1627f613b32cc7f2d2e8dcc48a4a61938b24f3d14fb77cee694f0c9311a";
              libraryHaskellDepends = [
                array base binary bytestring deepseq ghc-prim integer-gmp
              ];
              testHaskellDepends = [
                array base binary bytestring deepseq directory ghc-prim HUnit
                integer-gmp QuickCheck quickcheck-unicode random test-framework
                test-framework-hunit test-framework-quickcheck2
              ];
              doCheck = false;
              homepage = "https://github.com/haskell/text";
              description = "An efficient packed Unicode text type";
              license = stdenv.lib.licenses.bsd2;
              doHaddock = false;
            }
          )
          { inherit HUnit QuickCheck random; };
        th-abstraction = callPackage
          (
            { mkDerivation, base, containers, ghc-prim, stdenv
            , template-haskell
            }:
            mkDerivation {
              pname = "th-abstraction";
              version = "0.2.6.0";
              sha256 = "e52e289a547d68f203d65f2e63ec2d87a3c613007d2fe873615c0969b981823c";
              libraryHaskellDepends = [
                base containers ghc-prim template-haskell
              ];
              testHaskellDepends = [ base containers template-haskell ];
              homepage = "https://github.com/glguy/th-abstraction";
              description = "Nicer interface for reified information about data types";
              license = stdenv.lib.licenses.isc;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        th-lift = callPackage
          (
            { mkDerivation, base, ghc-prim, stdenv, template-haskell }:
            mkDerivation {
              pname = "th-lift";
              version = "0.7.8";
              sha256 = "2cf83385e848d9136a1d6e49ca845fd1d09935f2ff658c6f4e268d8ece02c12b";
              libraryHaskellDepends = [ base ghc-prim template-haskell ];
              testHaskellDepends = [ base ghc-prim template-haskell ];
              homepage = "http://github.com/mboes/th-lift";
              description = "Derive Template Haskell's Lift class for datatypes";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        time-locale-compat = callPackage
          (
            { mkDerivation, base, old-locale, stdenv, time }:
            mkDerivation {
              pname = "time-locale-compat";
              version = "0.1.1.3";
              sha256 = "9144bf68b47791a2ac73f45aeadbc5910be2da9ad174909e1a10a70b4576aced";
              libraryHaskellDepends = [ base old-locale time ];
              homepage = "https://github.com/khibino/haskell-time-locale-compat";
              description = "Compatibility of TimeLocale between old-locale and time-1.5";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        transformers-compat = callPackage
          (
            { mkDerivation, base, ghc-prim, stdenv, transformers }:
            mkDerivation {
              pname = "transformers-compat";
              version = "0.5.1.4";
              sha256 = "d881ef4ec164b631591b222efe7ff555af6d5397c9d86475b309ba9402a8ca9f";
              libraryHaskellDepends = [ base ghc-prim transformers ];
              homepage = "http://github.com/ekmett/transformers-compat/";
              description = "A small compatibility shim exposing the new types from transformers 0.3 and 0.4 to older Haskell platforms.";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        unix-compat = callPackage
          (
            { mkDerivation, base, stdenv, unix }:
            mkDerivation {
              pname = "unix-compat";
              version = "0.5.0.1";
              sha256 = "c2f299e0439c15d93d5700911c922fd2b35543c19ba053779cd52f3b051caebd";
              libraryHaskellDepends = [ base unix ];
              homepage = "http://github.com/jystic/unix-compat";
              description = "Portable POSIX-compatibility layer";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        unliftio-core = callPackage
          (
            { mkDerivation, base, stdenv, transformers }:
            mkDerivation {
              pname = "unliftio-core";
              version = "0.1.1.0";
              sha256 = "7550b017d87af53ae3e0d3b8524e24a77faf739073f35e40663454a9e9752385";
              libraryHaskellDepends = [ base transformers ];
              homepage = "https://github.com/fpco/unliftio/tree/master/unliftio-core#readme";
              description = "The MonadUnliftIO typeclass for unlifting monads to IO";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        void = callPackage
          (
            { mkDerivation, base, stdenv }:
            mkDerivation {
              pname = "void";
              version = "0.7.2";
              sha256 = "d3fffe66a03e4b53db1e459edf75ad8402385a817cae415d857ec0b03ce0cf2b";
              libraryHaskellDepends = [ base ];
              homepage = "http://github.com/ekmett/void";
              description = "A Haskell 98 logically uninhabited data type";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        zlib = callPackage
          (
            { mkDerivation, base, bytestring, QuickCheck, stdenv, tasty
            , tasty-hunit, tasty-quickcheck, zlib
            }:
            mkDerivation {
              pname = "zlib";
              version = "0.6.1.2";
              sha256 = "e4eb4e636caf07a16a9730ce469a00b65d5748f259f43edd904dd457b198a2bb";
              libraryHaskellDepends = [ base bytestring ];
              librarySystemDepends = [ zlib ];
              testHaskellDepends = [
                base bytestring QuickCheck tasty tasty-hunit tasty-quickcheck
              ];
              description = "Compression and decompression in the gzip and zlib formats";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit QuickCheck; inherit (nixpkgs) zlib; };
        gitrev = callPackage
          (
            { mkDerivation, base, base-compat, directory, filepath, process
            , stdenv, template-haskell
            }:
            mkDerivation {
              pname = "gitrev";
              version = "1.3.1";
              sha256 = "a89964db24f56727b0e7b10c98fe7c116d721d8c46f52d6e77088669aaa38332";
              libraryHaskellDepends = [
                base base-compat directory filepath process template-haskell
              ];
              homepage = "https://github.com/acfoltzer/gitrev";
              description = "Compile git revision info into Haskell projects";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit base-compat; };
        foundation = callPackage
          (
            { mkDerivation, base, basement, gauge, ghc-prim, stdenv }:
            mkDerivation {
              pname = "foundation";
              version = "0.0.19";
              sha256 = "b83bd852f1bc2f7a39fe02ce673580483cb3264ce10dd8768ee4dcf49a2b6f14";
              libraryHaskellDepends = [ base basement ghc-prim ];
              testHaskellDepends = [ base basement ];
              benchmarkHaskellDepends = [ base basement gauge ];
              homepage = "https://github.com/haskell-foundation/foundation";
              description = "Alternative prelude with batteries and no dependencies";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit basement; };
        logging-facade = callPackage
          (
            { mkDerivation, base, call-stack, hspec, stdenv, transformers }:
            mkDerivation {
              pname = "logging-facade";
              version = "0.3.0";
              sha256 = "8e7115258b76e0bf5d21b532dd916c63e79b32d1776cc355d2d184f67ae71434";
              libraryHaskellDepends = [ base call-stack transformers ];
              testHaskellDepends = [ base hspec ];
              homepage = "https://github.com/sol/logging-facade#readme";
              description = "Simple logging abstraction that allows multiple back-ends";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit call-stack hspec; };
        HUnit = callPackage
          (
            { mkDerivation, base, call-stack, deepseq, filepath, stdenv }:
            mkDerivation {
              pname = "HUnit";
              version = "1.6.0.0";
              sha256 = "7448e6b966e98e84b7627deba23f71b508e9a61e7bc571d74304a25d30e6d0de";
              libraryHaskellDepends = [ base call-stack deepseq ];
              testHaskellDepends = [ base call-stack deepseq filepath ];
              homepage = "https://github.com/hspec/HUnit#readme";
              description = "A unit testing framework for Haskell";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit call-stack; };
        ansi-terminal = callPackage
          (
            { mkDerivation, base, colour, stdenv }:
            mkDerivation {
              pname = "ansi-terminal";
              version = "0.8.0.1";
              sha256 = "1bd4355c176c48f85f9cd3728a8dbe45ad10111b71f5e2ffc606198b3d0f4659";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [ base colour ];
              executableHaskellDepends = [ base ];
              homepage = "https://github.com/feuerbach/ansi-terminal";
              description = "Simple ANSI terminal support, with Windows compatibility";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit colour; };
        logict = callPackage
          (
            { mkDerivation, base, mtl, stdenv }:
            mkDerivation {
              pname = "logict";
              version = "0.6.0.2";
              sha256 = "1182b68e8d00279460c7fb9b8284bf129805c07754c678b2a8de5a6d768e161e";
              libraryHaskellDepends = [ base mtl ];
              homepage = "http://code.haskell.org/~dolio/";
              description = "A backtracking logic-programming monad";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit mtl; };
        vector = callPackage
          (
            { mkDerivation, base, deepseq, ghc-prim, HUnit, primitive
            , QuickCheck, random, stdenv, template-haskell, test-framework
            , test-framework-hunit, test-framework-quickcheck2, transformers
            }:
            mkDerivation {
              pname = "vector";
              version = "0.12.0.1";
              sha256 = "b100ee79b9da2651276278cd3e0f08a3c152505cc52982beda507515af173d7b";
              revision = "2";
              editedCabalFile = "0vzr8kra73anchp86knkmkq2afkd1hw6hirldn9vn69frynb1n6y";
              libraryHaskellDepends = [ base deepseq ghc-prim primitive ];
              testHaskellDepends = [
                base HUnit QuickCheck random template-haskell test-framework
                test-framework-hunit test-framework-quickcheck2 transformers
              ];
              homepage = "https://github.com/haskell/vector";
              description = "Efficient Arrays";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit primitive QuickCheck random; };
        tf-random = callPackage
          (
            { mkDerivation, base, primitive, random, stdenv, time }:
            mkDerivation {
              pname = "tf-random";
              version = "0.5";
              sha256 = "2e30cec027b313c9e1794d326635d8fc5f79b6bf6e7580ab4b00186dadc88510";
              libraryHaskellDepends = [ base primitive random time ];
              description = "High-quality splittable pseudorandom number generator";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit primitive random; };
        StateVar = callPackage
          (
            { mkDerivation, base, stdenv, stm, transformers }:
            mkDerivation {
              pname = "StateVar";
              version = "1.1.0.4";
              sha256 = "7ad68decb5c9a76f83c95ece5fa13d1b053e4fb1079bd2d3538f6b05014dffb7";
              libraryHaskellDepends = [ base stm transformers ];
              homepage = "https://github.com/haskell-opengl/StateVar";
              description = "State variables";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit stm; };
        th-expand-syns = callPackage
          (
            { mkDerivation, base, containers, stdenv, syb, template-haskell }:
            mkDerivation {
              pname = "th-expand-syns";
              version = "0.4.4.0";
              sha256 = "cc0f52d1364ace9ba56f51afd9106a5fe01ed3f5ae45c958c1b0f83be0a6f906";
              libraryHaskellDepends = [ base containers syb template-haskell ];
              testHaskellDepends = [ base template-haskell ];
              homepage = "https://github.com/DanielSchuessler/th-expand-syns";
              description = "Expands type synonyms in Template Haskell ASTs";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit syb; };
        store-core = callPackage
          (
            { mkDerivation, base, bytestring, fail, ghc-prim, primitive, stdenv
            , text, transformers
            }:
            mkDerivation {
              pname = "store-core";
              version = "0.4.1";
              sha256 = "145285f9f26a64e9611e01749a0d569691a70fa898f5359bedcfca9dacb064b4";
              libraryHaskellDepends = [
                base bytestring fail ghc-prim primitive text transformers
              ];
              homepage = "https://github.com/fpco/store#readme";
              description = "Fast and lightweight binary serialization";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit fail primitive text; };
        parsec = callPackage
          (
            { mkDerivation, base, bytestring, HUnit, mtl, stdenv
            , test-framework, test-framework-hunit, text
            }:
            mkDerivation {
              pname = "parsec";
              version = "3.1.13.0";
              sha256 = "7861ae437a6177ee7c08899432fd8c062e7c110361da48a9f9e88263fd4d80f1";
              libraryHaskellDepends = [ base bytestring mtl text ];
              testHaskellDepends = [
                base HUnit mtl test-framework test-framework-hunit
              ];
              homepage = "https://github.com/hvr/parsec";
              description = "Monadic parser combinators";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit mtl text; };
        hashable = callPackage
          (
            { mkDerivation, base, bytestring, criterion, deepseq, ghc-prim
            , HUnit, integer-gmp, QuickCheck, random, siphash, stdenv
            , test-framework, test-framework-hunit, test-framework-quickcheck2
            , text, unix
            }:
            mkDerivation {
              pname = "hashable";
              version = "1.2.6.1";
              sha256 = "94ca8789e13bc05c1582c46b709f3b0f5aeec2092be634b8606dbd9c5915bb7a";
              revision = "2";
              editedCabalFile = "0w4756sa04nk2bw3vnysb0y9d09zzg3c77aydkjfxz1hnl1dvnjn";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [
                base bytestring deepseq ghc-prim integer-gmp text
              ];
              testHaskellDepends = [
                base bytestring ghc-prim HUnit QuickCheck random test-framework
                test-framework-hunit test-framework-quickcheck2 text unix
              ];
              benchmarkHaskellDepends = [
                base bytestring criterion ghc-prim integer-gmp siphash text
              ];
              homepage = "http://github.com/tibbe/hashable";
              description = "A class for types that can be converted to a hash value";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit QuickCheck random text; };
        blaze-builder = callPackage
          (
            { mkDerivation, base, bytestring, deepseq, HUnit, QuickCheck
            , stdenv, test-framework, test-framework-hunit
            , test-framework-quickcheck2, text, utf8-string
            }:
            mkDerivation {
              pname = "blaze-builder";
              version = "0.4.0.2";
              sha256 = "9ad3e4661bf5556d650fb9aa56a3ad6e6eec7575e87d472e8ab6d15eaef163d4";
              libraryHaskellDepends = [ base bytestring deepseq text ];
              testHaskellDepends = [
                base bytestring HUnit QuickCheck test-framework
                test-framework-hunit test-framework-quickcheck2 text utf8-string
              ];
              homepage = "http://github.com/lpsmith/blaze-builder";
              description = "Efficient buffered output";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit QuickCheck text; };
        transformers-base = callPackage
          (
            { mkDerivation, base, stdenv, stm, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "transformers-base";
              version = "0.4.4";
              sha256 = "6aa3494fc70659342fbbb163035d5827ecfd8079e3c929e2372adf771fd52387";
              revision = "1";
              editedCabalFile = "196pr3a4lhgklyw6nq6rv1j9djwzmvx7xrpp58carxnb55gk06pv";
              libraryHaskellDepends = [
                base stm transformers transformers-compat
              ];
              homepage = "https://github.com/mvv/transformers-base";
              description = "Lift computations from the bottom of a transformer stack";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit stm transformers-compat; };
        tagged = callPackage
          (
            { mkDerivation, base, deepseq, stdenv, template-haskell
            , transformers, transformers-compat
            }:
            mkDerivation {
              pname = "tagged";
              version = "0.8.5";
              sha256 = "e47c51c955ed77b0fa36897f652df990aa0a8c4eb278efaddcd604be00fc8d99";
              revision = "2";
              editedCabalFile = "0r2knfcq0b4s652vlvlnfwxlc2mkc2ra9kl8bp4zdn1awmfy0ia5";
              libraryHaskellDepends = [
                base deepseq template-haskell transformers transformers-compat
              ];
              homepage = "http://github.com/ekmett/tagged";
              description = "Haskell 98 phantom types to avoid unsafely passing dummy arguments";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit transformers-compat; };
        exceptions = callPackage
          (
            { mkDerivation, base, mtl, QuickCheck, stdenv, stm
            , template-haskell, test-framework, test-framework-quickcheck2
            , transformers, transformers-compat
            }:
            mkDerivation {
              pname = "exceptions";
              version = "0.8.3";
              sha256 = "4d6ad97e8e3d5dc6ce9ae68a469dc2fd3f66e9d312bc6faa7ab162eddcef87be";
              revision = "4";
              editedCabalFile = "18iip6wffnrp1jgnf09gxg4v17ymjank50kjshxvcy9s9l9g13ln";
              libraryHaskellDepends = [
                base mtl stm template-haskell transformers transformers-compat
              ];
              testHaskellDepends = [
                base mtl QuickCheck stm template-haskell test-framework
                test-framework-quickcheck2 transformers transformers-compat
              ];
              homepage = "http://github.com/ekmett/exceptions/";
              description = "Extensible optionally-pure exceptions";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit mtl QuickCheck stm transformers-compat; };
        Glob = callPackage
          (
            { mkDerivation, base, containers, directory, dlist, filepath, HUnit
            , QuickCheck, stdenv, test-framework, test-framework-hunit
            , test-framework-quickcheck2, transformers, transformers-compat
            }:
            mkDerivation {
              pname = "Glob";
              version = "0.9.1";
              sha256 = "80cb0b048d78f71ba5af1e58c8d651f5b6f1b37766d4da9b18e30a40edd4f567";
              libraryHaskellDepends = [
                base containers directory dlist filepath transformers
                transformers-compat
              ];
              testHaskellDepends = [
                base containers directory dlist filepath HUnit QuickCheck
                test-framework test-framework-hunit test-framework-quickcheck2
                transformers transformers-compat
              ];
              homepage = "http://iki.fi/matti.niemenmaa/glob/";
              description = "Globbing library";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit dlist HUnit QuickCheck transformers-compat; };
        memory = callPackage
          (
            { mkDerivation, base, basement, bytestring, deepseq, foundation
            , ghc-prim, stdenv, tasty, tasty-hunit, tasty-quickcheck
            }:
            mkDerivation {
              pname = "memory";
              version = "0.14.14";
              sha256 = "1d1b985620155dbacfc9d924b49505889a558f5a7787bf308fad418ded59960e";
              libraryHaskellDepends = [
                base basement bytestring deepseq foundation ghc-prim
              ];
              testHaskellDepends = [
                base basement bytestring foundation tasty tasty-hunit
                tasty-quickcheck
              ];
              homepage = "https://github.com/vincenthz/hs-memory";
              description = "memory and related abstraction stuff";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit basement foundation; };
        hspec-expectations = callPackage
          (
            { mkDerivation, base, call-stack, HUnit, nanospec, stdenv }:
            mkDerivation {
              pname = "hspec-expectations";
              version = "0.8.2";
              sha256 = "819607ea1faf35ce5be34be61c6f50f3389ea43892d56fb28c57a9f5d54fb4ef";
              libraryHaskellDepends = [ base call-stack HUnit ];
              testHaskellDepends = [ base call-stack HUnit nanospec ];
              homepage = "https://github.com/hspec/hspec-expectations#readme";
              description = "Catchy combinators for HUnit";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit call-stack HUnit; };
        smallcheck = callPackage
          (
            { mkDerivation, base, ghc-prim, logict, mtl, pretty, stdenv }:
            mkDerivation {
              pname = "smallcheck";
              version = "1.1.3.1";
              sha256 = "9ff5f16ffa4c4ab57c0f22fcada1825aa250c03f1559aae851d96738bb06bdd2";
              libraryHaskellDepends = [ base ghc-prim logict mtl pretty ];
              homepage = "https://github.com/feuerbach/smallcheck";
              description = "A property-based testing library";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit logict mtl; };
        vector-algorithms = callPackage
          (
            { mkDerivation, base, bytestring, containers, mtl, mwc-random
            , primitive, QuickCheck, stdenv, vector
            }:
            mkDerivation {
              pname = "vector-algorithms";
              version = "0.7.0.1";
              sha256 = "ed460a41ca068f568bc2027579ab14185fbb72c7ac469b5179ae5f8a52719070";
              revision = "1";
              editedCabalFile = "1996aj239vasr4hd5c0pi9i0bd08r6clzr76nqvf3hc5kjs7vml2";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [ base bytestring primitive vector ];
              executableHaskellDepends = [ base mtl mwc-random vector ];
              testHaskellDepends = [
                base bytestring containers QuickCheck vector
              ];
              homepage = "http://code.haskell.org/~dolio/";
              description = "Efficient algorithms for vector arrays";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit mtl primitive QuickCheck vector; };
        th-lift-instances = callPackage
          (
            { mkDerivation, base, bytestring, containers, QuickCheck, stdenv
            , template-haskell, text, th-lift, vector
            }:
            mkDerivation {
              pname = "th-lift-instances";
              version = "0.1.11";
              sha256 = "1da46afabdc73c86f279a0557d5a8f9af1296f9f6043264ba354b1c9cc65a6b8";
              libraryHaskellDepends = [
                base bytestring containers template-haskell text th-lift vector
              ];
              testHaskellDepends = [
                base bytestring containers QuickCheck template-haskell text vector
              ];
              homepage = "http://github.com/bennofs/th-lift-instances/";
              description = "Lift instances for template-haskell for common data types";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit QuickCheck text th-lift vector; };
        QuickCheck = callPackage
          (
            { mkDerivation, base, containers, deepseq, random, stdenv
            , template-haskell, tf-random, transformers
            }:
            mkDerivation {
              pname = "QuickCheck";
              version = "2.11.3";
              sha256 = "488c5652139da0bac8b3e7d76f11320ded298549e62db530938bfee9ca981876";
              libraryHaskellDepends = [
                base containers deepseq random template-haskell tf-random
                transformers
              ];
              testHaskellDepends = [ base ];
              homepage = "https://github.com/nick8325/quickcheck";
              description = "Automatic testing of Haskell programs";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit random tf-random; };
        contravariant = callPackage
          (
            { mkDerivation, base, StateVar, stdenv, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "contravariant";
              version = "1.4.1";
              sha256 = "c93d3d619fa378f3fdf92c53bb8b04b8f47963b88aba7cfa54b57656189ad0ed";
              libraryHaskellDepends = [
                base StateVar transformers transformers-compat
              ];
              homepage = "http://github.com/ekmett/contravariant/";
              description = "Contravariant functors";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit StateVar transformers-compat; };
        th-reify-many = callPackage
          (
            { mkDerivation, base, containers, mtl, safe, stdenv
            , template-haskell, th-expand-syns
            }:
            mkDerivation {
              pname = "th-reify-many";
              version = "0.1.8";
              sha256 = "cecaae187df911de515d08929e1394d6d6f7ce129795be8189a6b10d3734fe43";
              libraryHaskellDepends = [
                base containers mtl safe template-haskell th-expand-syns
              ];
              testHaskellDepends = [ base template-haskell ];
              homepage = "http://github.com/mgsloan/th-reify-many";
              description = "Recurseively reify template haskell datatype info";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit mtl safe th-expand-syns; };
        language-dot = callPackage
          (
            { mkDerivation, base, mtl, parsec, pretty, stdenv }:
            mkDerivation {
              pname = "language-dot";
              version = "0.1.0";
              sha256 = "15418f000c45efd129d98698d3258ff7996c66c7c9374072334868d2550b1581";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [ base mtl parsec pretty ];
              executableHaskellDepends = [ base mtl parsec pretty ];
              testHaskellDepends = [ base mtl parsec pretty ];
              description = "A library for the analysis and creation of Graphviz DOT files";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit mtl parsec; };
        uuid-types = callPackage
          (
            { mkDerivation, base, binary, bytestring, containers, criterion
            , deepseq, hashable, HUnit, QuickCheck, random, stdenv, tasty
            , tasty-hunit, tasty-quickcheck, text
            }:
            mkDerivation {
              pname = "uuid-types";
              version = "1.0.3";
              sha256 = "9276517ab24a9b06f39d6e3c33c6c2b4ace1fc2126dbc1cd9806866a6551b3fd";
              revision = "1";
              editedCabalFile = "0iwwj07gp28g357hv76k4h8pvlzamvchnw003cv3qk778pcpx201";
              libraryHaskellDepends = [
                base binary bytestring deepseq hashable random text
              ];
              testHaskellDepends = [
                base bytestring HUnit QuickCheck tasty tasty-hunit tasty-quickcheck
              ];
              benchmarkHaskellDepends = [
                base bytestring containers criterion deepseq random
              ];
              homepage = "https://github.com/aslatter/uuid";
              description = "Type definitions for Universally Unique Identifiers";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hashable HUnit QuickCheck random text; };
        unordered-containers = callPackage
          (
            { mkDerivation, base, bytestring, ChasingBottoms, containers
            , criterion, deepseq, deepseq-generics, hashable, hashmap, HUnit
            , mtl, QuickCheck, random, stdenv, test-framework
            , test-framework-hunit, test-framework-quickcheck2
            }:
            mkDerivation {
              pname = "unordered-containers";
              version = "0.2.9.0";
              sha256 = "6730cb5c4a3e953e2c199d6425be08fd088ff0089a3e140d63226c052e318250";
              libraryHaskellDepends = [ base deepseq hashable ];
              testHaskellDepends = [
                base ChasingBottoms containers hashable HUnit QuickCheck
                test-framework test-framework-hunit test-framework-quickcheck2
              ];
              benchmarkHaskellDepends = [
                base bytestring containers criterion deepseq deepseq-generics
                hashable hashmap mtl random
              ];
              homepage = "https://github.com/tibbe/unordered-containers";
              description = "Efficient hashing-based container types";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hashable HUnit mtl QuickCheck random; };
        scientific = callPackage
          (
            { mkDerivation, base, binary, bytestring, containers, criterion
            , deepseq, hashable, integer-gmp, integer-logarithms, primitive
            , QuickCheck, smallcheck, stdenv, tasty, tasty-ant-xml, tasty-hunit
            , tasty-quickcheck, tasty-smallcheck, text
            }:
            mkDerivation {
              pname = "scientific";
              version = "0.3.5.2";
              sha256 = "5ce479ff95482fb907267516bd0f8fff450bdeea546bbd1267fe035acf975657";
              revision = "4";
              editedCabalFile = "108m6b9w8l2q4r68mla9m5z47k6ahb0p68hypsmn140hgfr6a8la";
              libraryHaskellDepends = [
                base binary bytestring containers deepseq hashable integer-gmp
                integer-logarithms primitive text
              ];
              testHaskellDepends = [
                base binary bytestring QuickCheck smallcheck tasty tasty-ant-xml
                tasty-hunit tasty-quickcheck tasty-smallcheck text
              ];
              benchmarkHaskellDepends = [ base criterion ];
              homepage = "https://github.com/basvandijk/scientific";
              description = "Numbers represented using scientific notation";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hashable integer-logarithms primitive QuickCheck smallcheck text; };
        async = callPackage
          (
            { mkDerivation, base, hashable, HUnit, stdenv, stm, test-framework
            , test-framework-hunit
            }:
            mkDerivation {
              pname = "async";
              version = "2.2.1";
              sha256 = "8f0b86022a1319d3c1c68655790da4b7f98017982e27ec3f3dbfe01029d39027";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [ base hashable stm ];
              executableHaskellDepends = [ base stm ];
              testHaskellDepends = [
                base HUnit stm test-framework test-framework-hunit
              ];
              homepage = "https://github.com/simonmar/async";
              description = "Run IO operations asynchronously and wait for their results";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hashable HUnit stm; };
        monad-control = callPackage
          (
            { mkDerivation, base, stdenv, stm, transformers, transformers-base
            , transformers-compat
            }:
            mkDerivation {
              pname = "monad-control";
              version = "1.0.2.2";
              sha256 = "1e34a21d123f2ed8bb2708e7f30343fa1d9d7f36881f9871cbcca4bb07e7e433";
              libraryHaskellDepends = [
                base stm transformers transformers-base transformers-compat
              ];
              homepage = "https://github.com/basvandijk/monad-control";
              description = "Lift control operations, like exception catching, through monad transformers";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit stm transformers-base transformers-compat; };
        distributive = callPackage
          (
            { mkDerivation, base, base-orphans, Cabal, cabal-doctest, doctest
            , generic-deriving, hspec, stdenv, tagged, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "distributive";
              version = "0.5.3";
              sha256 = "9173805b9c941bda1f37e5aeb68ae30f57a12df9b17bd2aa86db3b7d5236a678";
              revision = "4";
              editedCabalFile = "1v6b2vnharppjn6w36lxfy0dvl93jzjq7fcyq9cp71650f1g9ai5";
              setupHaskellDepends = [ base Cabal cabal-doctest ];
              libraryHaskellDepends = [
                base base-orphans tagged transformers transformers-compat
              ];
              testHaskellDepends = [ base doctest generic-deriving hspec ];
              homepage = "http://github.com/ekmett/distributive/";
              description = "Distributive functors -- Dual to Traversable";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit base-orphans cabal-doctest hspec tagged transformers-compat; };
        temporary = callPackage
          (
            { mkDerivation, base, base-compat, directory, exceptions, filepath
            , stdenv, tasty, tasty-hunit, transformers, unix
            }:
            mkDerivation {
              pname = "temporary";
              version = "1.2.1.1";
              sha256 = "55772471e59529f4bde9555f6abb21d19a611c7d70b13befe114dc1a0ecb00f3";
              libraryHaskellDepends = [
                base directory exceptions filepath transformers unix
              ];
              testHaskellDepends = [
                base base-compat directory filepath tasty tasty-hunit unix
              ];
              homepage = "https://github.com/feuerbach/temporary";
              description = "Portable temporary file and directory support";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit base-compat exceptions; };
        resourcet = callPackage
          (
            { mkDerivation, base, containers, exceptions, hspec, mtl, primitive
            , stdenv, transformers, unliftio-core
            }:
            mkDerivation {
              pname = "resourcet";
              version = "1.2.0";
              sha256 = "095dc971a170f5cd2880e303ffb04a0feabeba29a1d776238b9691ece666fa26";
              libraryHaskellDepends = [
                base containers exceptions mtl primitive transformers unliftio-core
              ];
              testHaskellDepends = [ base hspec transformers ];
              homepage = "http://github.com/snoyberg/conduit";
              description = "Deterministic allocation and freeing of scarce resources";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit exceptions hspec mtl primitive unliftio-core; };
        cryptonite = callPackage
          (
            { mkDerivation, base, basement, bytestring, deepseq, gauge
            , ghc-prim, integer-gmp, memory, random, stdenv, tasty, tasty-hunit
            , tasty-kat, tasty-quickcheck
            }:
            mkDerivation {
              pname = "cryptonite";
              version = "0.25";
              sha256 = "89be1a18af8730a7bfe4d718d7d5f6ce858e9df93a411566d15bf992db5a3c8c";
              libraryHaskellDepends = [
                base basement bytestring deepseq ghc-prim integer-gmp memory
              ];
              testHaskellDepends = [
                base bytestring memory tasty tasty-hunit tasty-kat tasty-quickcheck
              ];
              benchmarkHaskellDepends = [
                base bytestring deepseq gauge memory random
              ];
              homepage = "https://github.com/haskell-crypto/cryptonite";
              description = "Cryptography Primitives sink";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit basement memory random; };
        quickcheck-io = callPackage
          (
            { mkDerivation, base, HUnit, QuickCheck, stdenv }:
            mkDerivation {
              pname = "quickcheck-io";
              version = "0.2.0";
              sha256 = "fb779119d79fe08ff4d502fb6869a70c9a8d5fd8ae0959f605c3c937efd96422";
              libraryHaskellDepends = [ base HUnit QuickCheck ];
              homepage = "https://github.com/hspec/quickcheck-io#readme";
              description = "Use HUnit assertions as QuickCheck properties";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit QuickCheck; };
        th-orphans = callPackage
          (
            { mkDerivation, base, hspec, hspec-discover, mtl, stdenv
            , template-haskell, th-lift, th-lift-instances, th-reify-many
            }:
            mkDerivation {
              pname = "th-orphans";
              version = "0.13.5";
              sha256 = "95644d4b7914317e1dd31095947b8371f1a18be0c09e75f0e29203eb774a25ad";
              libraryHaskellDepends = [
                base mtl template-haskell th-lift th-lift-instances th-reify-many
              ];
              testHaskellDepends = [ base hspec template-haskell ];
              testToolDepends = [ hspec-discover ];
              description = "Orphan instances for TH datatypes";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hspec hspec-discover mtl th-lift th-lift-instances th-reify-many; };
        mono-traversable = callPackage
          (
            { mkDerivation, base, bytestring, containers, foldl, gauge
            , hashable, hspec, HUnit, mwc-random, QuickCheck, semigroups, split
            , stdenv, text, transformers, unordered-containers, vector
            , vector-algorithms
            }:
            mkDerivation {
              pname = "mono-traversable";
              version = "1.0.8.1";
              sha256 = "991290797bd77ce2f2e23dd5dea32fb159c6cb9310615f64a0703ea4c6373935";
              libraryHaskellDepends = [
                base bytestring containers hashable split text transformers
                unordered-containers vector vector-algorithms
              ];
              testHaskellDepends = [
                base bytestring containers foldl hspec HUnit QuickCheck semigroups
                text transformers unordered-containers vector
              ];
              benchmarkHaskellDepends = [ base gauge mwc-random vector ];
              homepage = "https://github.com/snoyberg/mono-traversable#readme";
              description = "Type classes for mapping, folding, and traversing monomorphic containers";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hashable hspec HUnit QuickCheck semigroups split text unordered-containers vector vector-algorithms; };
        attoparsec = callPackage
          (
            { mkDerivation, array, base, bytestring, case-insensitive
            , containers, criterion, deepseq, directory, filepath, ghc-prim
            , http-types, parsec, QuickCheck, quickcheck-unicode, scientific
            , stdenv, tasty, tasty-quickcheck, text, transformers
            , unordered-containers, vector
            }:
            mkDerivation {
              pname = "attoparsec";
              version = "0.13.2.2";
              sha256 = "dd93471eb969172cc4408222a3842d867adda3dd7fb39ad8a4df1b121a67d848";
              libraryHaskellDepends = [
                array base bytestring containers deepseq scientific text
                transformers
              ];
              testHaskellDepends = [
                array base bytestring deepseq QuickCheck quickcheck-unicode
                scientific tasty tasty-quickcheck text transformers vector
              ];
              benchmarkHaskellDepends = [
                array base bytestring case-insensitive containers criterion deepseq
                directory filepath ghc-prim http-types parsec scientific text
                transformers unordered-containers vector
              ];
              homepage = "https://github.com/bos/attoparsec";
              description = "Fast combinator parsing for bytestrings and text";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit parsec QuickCheck scientific text unordered-containers vector; };
        streaming-commons = callPackage
          (
            { mkDerivation, array, async, base, blaze-builder, bytestring
            , deepseq, directory, gauge, hspec, network, process, QuickCheck
            , random, stdenv, stm, text, transformers, unix, zlib
            }:
            mkDerivation {
              pname = "streaming-commons";
              version = "0.1.19";
              sha256 = "43fcae90df5548d9968b31371f13ec7271df86ac34a484c094616867ed4217a7";
              libraryHaskellDepends = [
                array async base blaze-builder bytestring directory network process
                random stm text transformers unix zlib
              ];
              testHaskellDepends = [
                array async base blaze-builder bytestring deepseq hspec network
                QuickCheck text unix zlib
              ];
              benchmarkHaskellDepends = [
                base blaze-builder bytestring deepseq gauge text
              ];
              homepage = "https://github.com/fpco/streaming-commons";
              description = "Common lower-level functions needed by various streaming data libraries";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit async blaze-builder hspec network QuickCheck random stm text zlib; };
        lifted-base = callPackage
          (
            { mkDerivation, base, criterion, HUnit, monad-control, monad-peel
            , stdenv, test-framework, test-framework-hunit, transformers
            , transformers-base, transformers-compat
            }:
            mkDerivation {
              pname = "lifted-base";
              version = "0.2.3.11";
              sha256 = "8ec47a9fce7cf5913766a5c53e1b2cf254be733fa9d62e6e2f3f24e538005aab";
              revision = "1";
              editedCabalFile = "0vrik0j1xv2yp759ffa7jb7q838z4wglnbgsrja97mx0dwsbavnx";
              libraryHaskellDepends = [ base monad-control transformers-base ];
              testHaskellDepends = [
                base HUnit monad-control test-framework test-framework-hunit
                transformers transformers-base transformers-compat
              ];
              benchmarkHaskellDepends = [
                base criterion monad-control monad-peel transformers
              ];
              homepage = "https://github.com/basvandijk/lifted-base";
              description = "lifted IO operations from the base library";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit monad-control transformers-base transformers-compat; };
        comonad = callPackage
          (
            { mkDerivation, base, Cabal, cabal-doctest, containers
            , contravariant, distributive, doctest, semigroups, stdenv, tagged
            , transformers, transformers-compat
            }:
            mkDerivation {
              pname = "comonad";
              version = "5.0.3";
              sha256 = "a7f4584d634051123c547f0d10f88eaf23d99229dbd01dfdcd98cddd41e54df6";
              revision = "1";
              editedCabalFile = "1i72zgxjkbldkwz0g2awf44cm9466ahll89j5kl45vszx4iz0anl";
              setupHaskellDepends = [ base Cabal cabal-doctest ];
              libraryHaskellDepends = [
                base containers contravariant distributive semigroups tagged
                transformers transformers-compat
              ];
              testHaskellDepends = [ base doctest ];
              homepage = "http://github.com/ekmett/comonad/";
              description = "Comonads";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit cabal-doctest contravariant distributive semigroups tagged transformers-compat; };
        mockery = callPackage
          (
            { mkDerivation, base, base-compat, bytestring, directory, filepath
            , hspec, logging-facade, stdenv, temporary
            }:
            mkDerivation {
              pname = "mockery";
              version = "0.3.5";
              sha256 = "b7a1edacd3d32dc7f0e28c67877209d3ca3551d1da186f6445f825f3477dd727";
              libraryHaskellDepends = [
                base base-compat bytestring directory filepath logging-facade
                temporary
              ];
              testHaskellDepends = [
                base base-compat bytestring directory filepath hspec logging-facade
                temporary
              ];
              description = "Support functions for automated testing";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit base-compat hspec logging-facade temporary; };
        cryptohash = callPackage
          (
            { mkDerivation, base, byteable, bytestring, criterion, cryptonite
            , ghc-prim, HUnit, memory, QuickCheck, stdenv, tasty, tasty-hunit
            , tasty-quickcheck
            }:
            mkDerivation {
              pname = "cryptohash";
              version = "0.11.9";
              sha256 = "c28f847fc1fcd65b6eea2e74a100300af940919f04bb21d391f6a773968f22fb";
              libraryHaskellDepends = [
                base byteable bytestring cryptonite ghc-prim memory
              ];
              testHaskellDepends = [
                base byteable bytestring HUnit QuickCheck tasty tasty-hunit
                tasty-quickcheck
              ];
              benchmarkHaskellDepends = [ base byteable bytestring criterion ];
              homepage = "http://github.com/vincenthz/hs-cryptohash";
              description = "collection of crypto hashes, fast, pure and practical";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit byteable cryptonite HUnit memory QuickCheck; };
        hspec-core = callPackage
          (
            { mkDerivation, ansi-terminal, array, base, call-stack, deepseq
            , directory, filepath, hspec-expectations, hspec-meta, HUnit
            , process, QuickCheck, quickcheck-io, random, setenv, silently
            , stdenv, stm, temporary, tf-random, time, transformers
            }:
            mkDerivation {
              pname = "hspec-core";
              version = "2.4.8";
              sha256 = "24ca82ca29cf9379c24133f510decc5dd1dbe447c3a9bc82dbcc365c8f35f90b";
              libraryHaskellDepends = [
                ansi-terminal array base call-stack deepseq directory filepath
                hspec-expectations HUnit QuickCheck quickcheck-io random setenv stm
                tf-random time transformers
              ];
              testHaskellDepends = [
                ansi-terminal array base call-stack deepseq directory filepath
                hspec-expectations hspec-meta HUnit process QuickCheck
                quickcheck-io random setenv silently stm temporary tf-random time
                transformers
              ];
              testTarget = "--test-option=--skip --test-option='Test.Hspec.Core.Runner.hspecResult runs specs in parallel'";
              homepage = "http://hspec.github.io/";
              description = "A Testing Framework for Haskell";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit ansi-terminal call-stack hspec-expectations HUnit QuickCheck quickcheck-io random setenv stm temporary tf-random; };
        th-utilities = callPackage
          (
            { mkDerivation, base, bytestring, containers, directory, filepath
            , hspec, primitive, stdenv, syb, template-haskell, text, th-orphans
            , vector
            }:
            mkDerivation {
              pname = "th-utilities";
              version = "0.2.0.1";
              sha256 = "65c64cee69c0d9bf8d0d5d4590aaea7dcf4177f97818526cbb3fac20901671d6";
              libraryHaskellDepends = [
                base bytestring containers directory filepath primitive syb
                template-haskell text th-orphans
              ];
              testHaskellDepends = [
                base bytestring containers directory filepath hspec primitive syb
                template-haskell text th-orphans vector
              ];
              homepage = "https://github.com/fpco/th-utilities#readme";
              description = "Collection of useful functions for use with Template Haskell";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hspec primitive syb text th-orphans vector; };
        conduit = callPackage
          (
            { mkDerivation, base, bytestring, containers, deepseq, directory
            , exceptions, filepath, gauge, hspec, kan-extensions
            , mono-traversable, mtl, mwc-random, primitive, QuickCheck
            , resourcet, safe, silently, split, stdenv, text, transformers
            , unix, unliftio, unliftio-core, vector
            }:
            mkDerivation {
              pname = "conduit";
              version = "1.3.0";
              sha256 = "d7cff0a7ee0de8661457cf7d209aa9b6ff9f24453be4abf0625c51b47cbb4094";
              libraryHaskellDepends = [
                base bytestring directory exceptions filepath mono-traversable mtl
                primitive resourcet text transformers unix unliftio-core vector
              ];
              testHaskellDepends = [
                base bytestring containers directory exceptions filepath hspec
                mono-traversable mtl QuickCheck resourcet safe silently split text
                transformers unliftio vector
              ];
              benchmarkHaskellDepends = [
                base containers deepseq gauge hspec kan-extensions mwc-random
                transformers vector
              ];
              homepage = "http://github.com/snoyberg/conduit";
              description = "Streaming data processing library";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit exceptions hspec mono-traversable mtl primitive QuickCheck resourcet safe split text unliftio-core vector; };
        aeson = callPackage
          (
            { mkDerivation, attoparsec, base, base-compat, base-orphans
            , base16-bytestring, bytestring, containers, deepseq, directory
            , dlist, filepath, generic-deriving, ghc-prim, hashable
            , hashable-time, HUnit, integer-logarithms, QuickCheck
            , quickcheck-instances, scientific, stdenv, tagged
            , template-haskell, test-framework, test-framework-hunit
            , test-framework-quickcheck2, text, th-abstraction, time
            , time-locale-compat, unordered-containers, uuid-types, vector
            }:
            mkDerivation {
              pname = "aeson";
              version = "1.2.4.0";
              sha256 = "3401dba4fddb92c8a971f6645b38e2f8a1b286ef7061cd392a1a567640bbfc9b";
              libraryHaskellDepends = [
                attoparsec base base-compat bytestring containers deepseq dlist
                ghc-prim hashable scientific tagged template-haskell text
                th-abstraction time time-locale-compat unordered-containers
                uuid-types vector
              ];
              testHaskellDepends = [
                attoparsec base base-compat base-orphans base16-bytestring
                bytestring containers directory dlist filepath generic-deriving
                ghc-prim hashable hashable-time HUnit integer-logarithms QuickCheck
                quickcheck-instances scientific tagged template-haskell
                test-framework test-framework-hunit test-framework-quickcheck2 text
                time time-locale-compat unordered-containers uuid-types vector
              ];
              homepage = "https://github.com/bos/aeson";
              description = "Fast JSON parsing and encoding";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit attoparsec base-compat base-orphans dlist hashable HUnit integer-logarithms QuickCheck scientific tagged text th-abstraction time-locale-compat unordered-containers uuid-types vector; };
        bifunctors = callPackage
          (
            { mkDerivation, base, base-orphans, comonad, containers, hspec
            , hspec-discover, QuickCheck, semigroups, stdenv, tagged
            , template-haskell, th-abstraction, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "bifunctors";
              version = "5.5.2";
              sha256 = "332bb2ea19e77dac55282daff8046d89f69514ced5b987779d887e53b5d7cb11";
              libraryHaskellDepends = [
                base base-orphans comonad containers semigroups tagged
                template-haskell th-abstraction transformers transformers-compat
              ];
              testHaskellDepends = [
                base hspec QuickCheck template-haskell transformers
                transformers-compat
              ];
              testToolDepends = [ hspec-discover ];
              homepage = "http://github.com/ekmett/bifunctors/";
              description = "Bifunctors";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit base-orphans comonad hspec hspec-discover QuickCheck semigroups tagged th-abstraction transformers-compat; };
        hspec-smallcheck = callPackage
          (
            { mkDerivation, base, call-stack, hspec, hspec-core, HUnit
            , QuickCheck, smallcheck, stdenv
            }:
            mkDerivation {
              pname = "hspec-smallcheck";
              version = "0.5.0";
              sha256 = "353c74dce3c42f17d012bea96b62e88ca21b0b24ac14d0daf1a5a08a4b02ce51";
              libraryHaskellDepends = [
                base call-stack hspec-core HUnit smallcheck
              ];
              testHaskellDepends = [
                base call-stack hspec hspec-core HUnit QuickCheck smallcheck
              ];
              homepage = "http://hspec.github.io/";
              description = "SmallCheck support for the Hspec testing framework";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit call-stack hspec hspec-core HUnit QuickCheck smallcheck; };
        hspec = callPackage
          (
            { mkDerivation, base, call-stack, directory, hspec-core
            , hspec-discover, hspec-expectations, hspec-meta, HUnit, QuickCheck
            , stdenv, stringbuilder, transformers
            }:
            mkDerivation {
              pname = "hspec";
              version = "2.4.8";
              sha256 = "94d4e0d688db1c62791c33b35cffc7b17f5a2d43387e1bb20d2b18f3dd6ceda2";
              libraryHaskellDepends = [
                base call-stack hspec-core hspec-discover hspec-expectations HUnit
                QuickCheck transformers
              ];
              testHaskellDepends = [
                base call-stack directory hspec-core hspec-discover
                hspec-expectations hspec-meta HUnit QuickCheck stringbuilder
                transformers
              ];
              homepage = "http://hspec.github.io/";
              description = "A Testing Framework for Haskell";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit call-stack hspec-core hspec-discover hspec-expectations HUnit QuickCheck; };
        yaml = callPackage
          (
            { mkDerivation, aeson, attoparsec, base, base-compat, bytestring
            , conduit, containers, directory, filepath, hspec, HUnit, libyaml
            , mockery, resourcet, scientific, semigroups, stdenv
            , template-haskell, temporary, text, transformers
            , unordered-containers, vector
            }:
            mkDerivation {
              pname = "yaml";
              version = "0.8.28";
              sha256 = "f702b6a489ad94cda3c0cb15db34a30356d7b2cdc86a4d0f5340f2ece69f8f6b";
              revision = "1";
              editedCabalFile = "0f8vb5v0xfpsc02zqh9pzgv4fir93sgijk342lz5k872gscfjn62";
              configureFlags = [ "-fsystem-libyaml" ];
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [
                aeson attoparsec base bytestring conduit containers directory
                filepath resourcet scientific semigroups template-haskell text
                transformers unordered-containers vector
              ];
              libraryPkgconfigDepends = [ libyaml ];
              executableHaskellDepends = [ aeson base bytestring ];
              testHaskellDepends = [
                aeson base base-compat bytestring conduit directory hspec HUnit
                mockery resourcet temporary text transformers unordered-containers
                vector
              ];
              homepage = "http://github.com/snoyberg/yaml/";
              description = "Support for parsing and rendering YAML documents";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit aeson attoparsec base-compat conduit hspec HUnit mockery resourcet scientific semigroups temporary text unordered-containers vector; };
        semigroupoids = callPackage
          (
            { mkDerivation, base, base-orphans, bifunctors, Cabal
            , cabal-doctest, comonad, containers, contravariant, distributive
            , doctest, hashable, semigroups, stdenv, tagged, template-haskell
            , transformers, transformers-compat, unordered-containers
            }:
            mkDerivation {
              pname = "semigroupoids";
              version = "5.2.2";
              sha256 = "e4def54834cda65ac1e74e6f12a435410e19c1348e820434a30c491c8937299e";
              revision = "1";
              editedCabalFile = "16pf83y17jbjbqv6rqlz4icdzsv6b10vjci6pf92y7cpizzjw0sy";
              setupHaskellDepends = [ base Cabal cabal-doctest ];
              libraryHaskellDepends = [
                base base-orphans bifunctors comonad containers contravariant
                distributive hashable semigroups tagged template-haskell
                transformers transformers-compat unordered-containers
              ];
              testHaskellDepends = [ base doctest ];
              homepage = "http://github.com/ekmett/semigroupoids";
              description = "Semigroupoids: Category sans id";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit base-orphans bifunctors cabal-doctest comonad contravariant distributive hashable semigroups tagged transformers-compat unordered-containers; };
        profunctors = callPackage
          (
            { mkDerivation, base, base-orphans, bifunctors, comonad
            , contravariant, distributive, semigroups, stdenv, tagged
            , transformers
            }:
            mkDerivation {
              pname = "profunctors";
              version = "5.2.2";
              sha256 = "e981e6a33ac99d38a947a749179bbea3c294ecf6bfde41660fe6d8d5a2e43768";
              libraryHaskellDepends = [
                base base-orphans bifunctors comonad contravariant distributive
                semigroups tagged transformers
              ];
              homepage = "http://github.com/ekmett/profunctors/";
              description = "Profunctors";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit base-orphans bifunctors comonad contravariant distributive semigroups tagged; };
        hpack = callPackage
          (
            { mkDerivation, aeson, base, bifunctors, bytestring, Cabal
            , containers, cryptonite, deepseq, directory, filepath, Glob, hspec
            , HUnit, interpolate, mockery, pretty, QuickCheck, scientific
            , stdenv, temporary, text, transformers, unordered-containers, yaml
            }:
            mkDerivation {
              pname = "hpack";
              version = "0.21.2";
              sha256 = "0c547729a2b6a49dd4a2cf32b737667ab94b8745e8648b375b827c1488c83abf";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [
                aeson base bifunctors bytestring Cabal containers cryptonite
                deepseq directory filepath Glob pretty scientific text transformers
                unordered-containers yaml
              ];
              executableHaskellDepends = [
                aeson base bifunctors bytestring Cabal containers cryptonite
                deepseq directory filepath Glob pretty scientific text transformers
                unordered-containers yaml
              ];
              testHaskellDepends = [
                aeson base bifunctors bytestring Cabal containers cryptonite
                deepseq directory filepath Glob hspec HUnit interpolate mockery
                pretty QuickCheck scientific temporary text transformers
                unordered-containers yaml
              ];
              homepage = "https://github.com/sol/hpack#readme";
              description = "An alternative format for Haskell packages";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit aeson bifunctors cryptonite Glob hspec HUnit mockery QuickCheck scientific temporary text unordered-containers yaml; };
        free = callPackage
          (
            { mkDerivation, base, bifunctors, comonad, containers, distributive
            , exceptions, mtl, profunctors, semigroupoids, semigroups, stdenv
            , template-haskell, transformers, transformers-base
            , transformers-compat
            }:
            mkDerivation {
              pname = "free";
              version = "5";
              sha256 = "87916bda2ae9766c1b1b35d4fe3ed3c1bcb587e61f783776af4c5b4a2adf8ae8";
              libraryHaskellDepends = [
                base bifunctors comonad containers distributive exceptions mtl
                profunctors semigroupoids semigroups template-haskell transformers
                transformers-base transformers-compat
              ];
              homepage = "http://github.com/ekmett/free/";
              description = "Monads for free";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit bifunctors comonad distributive exceptions mtl profunctors semigroupoids semigroups transformers-base transformers-compat; };
        store = callPackage
          (
            { mkDerivation, array, async, base, base-orphans, base64-bytestring
            , bytestring, cereal, cereal-vector, conduit, containers
            , contravariant, criterion, cryptohash, deepseq, directory
            , filepath, free, ghc-prim, hashable, hspec, hspec-smallcheck
            , integer-gmp, lifted-base, monad-control, mono-traversable
            , network, primitive, resourcet, safe, semigroups, smallcheck
            , stdenv, store-core, streaming-commons, syb, template-haskell
            , text, th-lift, th-lift-instances, th-orphans, th-reify-many
            , th-utilities, time, transformers, unordered-containers, vector
            , vector-binary-instances, void, weigh
            }:
            mkDerivation {
              pname = "store";
              version = "0.4.3.2";
              sha256 = "eca47c14b14ce5a6369a4b09a048b5a7fe7574d3f1b1099bc03449416c80308e";
              libraryHaskellDepends = [
                array async base base-orphans base64-bytestring bytestring conduit
                containers contravariant cryptohash deepseq directory filepath free
                ghc-prim hashable hspec hspec-smallcheck integer-gmp lifted-base
                monad-control mono-traversable network primitive resourcet safe
                semigroups smallcheck store-core streaming-commons syb
                template-haskell text th-lift th-lift-instances th-orphans
                th-reify-many th-utilities time transformers unordered-containers
                vector void
              ];
              testHaskellDepends = [
                array async base base-orphans base64-bytestring bytestring cereal
                cereal-vector conduit containers contravariant criterion cryptohash
                deepseq directory filepath free ghc-prim hashable hspec
                hspec-smallcheck integer-gmp lifted-base monad-control
                mono-traversable network primitive resourcet safe semigroups
                smallcheck store-core streaming-commons syb template-haskell text
                th-lift th-lift-instances th-orphans th-reify-many th-utilities
                time transformers unordered-containers vector
                vector-binary-instances void weigh
              ];
              benchmarkHaskellDepends = [
                array async base base-orphans base64-bytestring bytestring conduit
                containers contravariant criterion cryptohash deepseq directory
                filepath free ghc-prim hashable hspec hspec-smallcheck integer-gmp
                lifted-base monad-control mono-traversable network primitive
                resourcet safe semigroups smallcheck store-core streaming-commons
                syb template-haskell text th-lift th-lift-instances th-orphans
                th-reify-many th-utilities time transformers unordered-containers
                vector void
              ];
              homepage = "https://github.com/fpco/store#readme";
              description = "Fast binary serialization";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit async base-orphans base64-bytestring conduit contravariant cryptohash free hashable hspec hspec-smallcheck lifted-base monad-control mono-traversable network primitive resourcet safe semigroups smallcheck store-core streaming-commons syb text th-lift th-lift-instances th-orphans th-reify-many th-utilities unordered-containers vector void; };
      };

      newResolver = compiler.override {
        overrides = overrideFunction;
      };

    in newResolver;
}
