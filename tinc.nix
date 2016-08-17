{ nixpkgs }:
rec {
  compiler = nixpkgs.haskellPackages;
  resolver =
    let
      callPackage = compiler.callPackage;

      overrideFunction = self: super: rec {
        HUnit = callPackage
          (
            { mkDerivation, base, deepseq, filepath, stdenv }:
            mkDerivation {
              pname = "HUnit";
              version = "1.3.1.1";
              sha256 = "1y4fv8r7xi302ahj6p71hvsgz3rkb2c4vw09j935ns5bj11grrck";
              libraryHaskellDepends = [ base deepseq ];
              testHaskellDepends = [ base deepseq filepath ];
              homepage = "https://github.com/hspec/HUnit#readme";
              description = "A unit testing framework for Haskell";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        ansi-terminal = callPackage
          (
            { mkDerivation, base, stdenv, unix }:
            mkDerivation {
              pname = "ansi-terminal";
              version = "0.6.2.3";
              sha256 = "0hpfw0k025y681m9ml1c712skrb1p4vh7z5x1f0ci9ww7ssjrh2d";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [ base unix ];
              executableHaskellDepends = [ base unix ];
              homepage = "https://github.com/feuerbach/ansi-terminal";
              description = "Simple ANSI terminal support, with Windows compatibility";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        base-compat = callPackage
          (
            { mkDerivation, base, hspec, QuickCheck, stdenv, unix }:
            mkDerivation {
              pname = "base-compat";
              version = "0.9.1";
              sha256 = "0jj6nq0vb8ap3724c3r3cavc298m1gm238vmgi7wzzxr8s0v8cqh";
              libraryHaskellDepends = [ base unix ];
              testHaskellDepends = [ base hspec QuickCheck ];
              description = "A compatibility layer for base";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hspec QuickCheck; };
        dlist = callPackage
          (
            { mkDerivation, base, Cabal, deepseq, QuickCheck, stdenv }:
            mkDerivation {
              pname = "dlist";
              version = "0.8.0.1";
              sha256 = "1rlq492b6kw58wrkpp5islfdfq3dl4rl32lcal7ik50bpnc9hp9v";
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
            { mkDerivation, base, stdenv }:
            mkDerivation {
              pname = "fail";
              version = "4.9.0.0";
              sha256 = "18nlj6xvnggy61gwbyrpmvbdkq928wv0wx2zcsljb52kbhddnp3d";
              libraryHaskellDepends = [ base ];
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
            { mkDerivation, base, stdenv, unix }:
            mkDerivation {
              pname = "filelock";
              version = "0.1.0.1";
              sha256 = "0qypjnbkfayqyaymx8qrq4abddlrlzanf6lqhfn9cqzcgzr6735d";
              libraryHaskellDepends = [ base unix ];
              homepage = "http://github.com/takano-akio/filelock";
              description = "Portable interface to file locking (flock / LockFileEx)";
              license = stdenv.lib.licenses.publicDomain;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        gitrev = callPackage
          (
            { mkDerivation, base, directory, filepath, process, stdenv
            , template-haskell
            }:
            mkDerivation {
              pname = "gitrev";
              version = "1.2.0";
              sha256 = "00ii00j5bnxnhnmzcsbqfin8kdj6n9ll7akg3j8apajwvd7f74a3";
              libraryHaskellDepends = [
                base directory filepath process template-haskell
              ];
              homepage = "https://github.com/acfoltzer/gitrev";
              description = "Compile git revision info into Haskell projects";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        graph-wrapper = callPackage
          (
            { mkDerivation, array, base, containers, deepseq, hspec, QuickCheck
            , stdenv
            }:
            mkDerivation {
              pname = "graph-wrapper";
              version = "0.2.5.1";
              sha256 = "04z1qbsf1c31r0mhn8bgr8hisffxacq3j61y4fym28idr8zqaqc3";
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
            { mkDerivation, base, directory, filepath, hspec-meta, stdenv }:
            mkDerivation {
              pname = "hspec-discover";
              version = "2.2.3";
              sha256 = "0bx9nlc07vihkm0ykfz2fcwd5v6zszb1mw81mczi72k2mpbm6q6w";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [ base directory filepath ];
              executableHaskellDepends = [ base directory filepath ];
              testHaskellDepends = [ base directory filepath hspec-meta ];
              homepage = "http://hspec.github.io/";
              description = "Automatically discover and run Hspec tests";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        hspec-expectations = callPackage
          (
            { mkDerivation, base, HUnit, stdenv }:
            mkDerivation {
              pname = "hspec-expectations";
              version = "0.7.2";
              sha256 = "1w56jiqfyl237sr207gh3b0l8sr9layy0mdsgd5wknzb49mif6ip";
              libraryHaskellDepends = [ base HUnit ];
              homepage = "https://github.com/sol/hspec-expectations#readme";
              description = "Catchy combinators for HUnit";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit; };
        logging-facade = callPackage
          (
            { mkDerivation, base, hspec, stdenv, template-haskell, transformers
            }:
            mkDerivation {
              pname = "logging-facade";
              version = "0.1.1";
              sha256 = "18ldv6rsff480rqpbs3iabjpvn1fhw0i2a0g80jnhxg9ajfz5yb0";
              libraryHaskellDepends = [ base template-haskell transformers ];
              testHaskellDepends = [ base hspec ];
              description = "Simple logging abstraction that allows multiple back-ends";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hspec; };
        mtl = callPackage
          (
            { mkDerivation, base, stdenv, transformers }:
            mkDerivation {
              pname = "mtl";
              version = "2.2.1";
              sha256 = "1icdbj2rshzn0m1zz5wa7v3xvkf6qw811p4s7jgqwvx1ydwrvrfa";
              revision = "1";
              editedCabalFile = "4b5a800fe9edf168fc7ae48c7a3fc2aab6b418ac15be2f1dad43c0f48a494a3b";
              libraryHaskellDepends = [ base transformers ];
              homepage = "http://github.com/ekmett/mtl";
              description = "Monad classes, using functional dependencies";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        primitive = callPackage
          (
            { mkDerivation, base, ghc-prim, stdenv, transformers }:
            mkDerivation {
              pname = "primitive";
              version = "0.6.1.0";
              sha256 = "1j1q7l21rdm8kfs93vibr3xwkkhqis181w2k6klfhx5g5skiywwk";
              revision = "1";
              editedCabalFile = "6ec7c2455c437aba71f856b797e7db440c83719509aa63a9a3d1b4652ca3683d";
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
              sha256 = "0nis3lbkp8vfx8pkr6v7b7kr5m334bzb0fk9vxqklnp2aw8a865p";
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
            { mkDerivation, base, stdenv }:
            mkDerivation {
              pname = "safe";
              version = "0.3.9";
              sha256 = "1jdnp5zhvalf1xy8i872n29nljfjz6lnl9ghj80ffisrnnkrwcfh";
              libraryHaskellDepends = [ base ];
              homepage = "https://github.com/ndmitchell/safe#readme";
              description = "Library of safe (exception free) functions";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        semigroups = callPackage
          (
            { mkDerivation, base, stdenv }:
            mkDerivation {
              pname = "semigroups";
              version = "0.18.2";
              sha256 = "1r6hsn3am3dpf4rprrj4m04d9318v9iq02bin0pl29dg4a3gzjax";
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
              sha256 = "0cnbgrvb9byyahb37zlqrj05rj25v190crgcw8wmlgf0mwwxyn73";
              revision = "1";
              editedCabalFile = "c5916ac0d2a828473cd171261328a290afe0abd799db1ac8c310682fe778c45b";
              libraryHaskellDepends = [ base unix ];
              description = "A cross-platform library for setting environment variables";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        stm = callPackage
          (
            { mkDerivation, array, base, stdenv }:
            mkDerivation {
              pname = "stm";
              version = "2.4.4.1";
              sha256 = "111kpy1d6f5c0bggh6hyfm86q5p8bq1qbqf6dw2x4l4dxnar16cg";
              libraryHaskellDepends = [ array base ];
              description = "Software Transactional Memory";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        async = callPackage
          (
            { mkDerivation, base, HUnit, stdenv, stm, test-framework
            , test-framework-hunit
            }:
            mkDerivation {
              pname = "async";
              version = "2.1.0";
              sha256 = "0brcy9bxhy0kxwvh3sfahgd2bg3zgbkhm5nrikf5r2y6z48pdhwk";
              libraryHaskellDepends = [ base stm ];
              testHaskellDepends = [
                base HUnit test-framework test-framework-hunit
              ];
              homepage = "https://github.com/simonmar/async";
              description = "Run IO operations asynchronously and wait for their results";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit stm; };
        syb = callPackage
          (
            { mkDerivation, base, containers, HUnit, mtl, stdenv }:
            mkDerivation {
              pname = "syb";
              version = "0.6";
              sha256 = "1p3cnqjm13677r4a966zffzhi9b3a321aln8zs8ckqj0d9z1z3d3";
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
              version = "1.2.2.1";
              sha256 = "0nrrzx0ws7pv4dx9jbc6jm2734al1cr0m6iwcnbck4v2yfyv3p8s";
              libraryHaskellDepends = [
                array base binary bytestring deepseq ghc-prim integer-gmp
              ];
              testHaskellDepends = [
                array base binary bytestring deepseq directory ghc-prim HUnit
                integer-gmp QuickCheck quickcheck-unicode random test-framework
                test-framework-hunit test-framework-quickcheck2
              ];
              doCheck = false;
              homepage = "https://github.com/bos/text";
              description = "An efficient packed Unicode text type";
              license = stdenv.lib.licenses.bsd3;
              doHaddock = false;
            }
          )
          { inherit HUnit QuickCheck random; };
        hashable = callPackage
          (
            { mkDerivation, base, bytestring, ghc-prim, HUnit, integer-gmp
            , QuickCheck, random, stdenv, test-framework, test-framework-hunit
            , test-framework-quickcheck2, text, unix
            }:
            mkDerivation {
              pname = "hashable";
              version = "1.2.4.0";
              sha256 = "1wrwpchksxd1i63ydpqy6jkdzxrb5qsy64rfiv9lik9r1kdp35pv";
              libraryHaskellDepends = [
                base bytestring ghc-prim integer-gmp text
              ];
              testHaskellDepends = [
                base bytestring ghc-prim HUnit QuickCheck random test-framework
                test-framework-hunit test-framework-quickcheck2 text unix
              ];
              homepage = "http://github.com/tibbe/hashable";
              description = "A class for types that can be converted to a hash value";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit QuickCheck random text; };
        parsec = callPackage
          (
            { mkDerivation, base, bytestring, HUnit, mtl, stdenv
            , test-framework, test-framework-hunit, text
            }:
            mkDerivation {
              pname = "parsec";
              version = "3.1.11";
              sha256 = "0vk7q9j2128q191zf1sg0ylj9s9djwayqk9747k0a5fin4f2b1vg";
              libraryHaskellDepends = [ base bytestring mtl text ];
              testHaskellDepends = [
                base HUnit test-framework test-framework-hunit
              ];
              homepage = "https://github.com/aslatter/parsec";
              description = "Monadic parser combinators";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit mtl text; };
        language-dot = callPackage
          (
            { mkDerivation, base, mtl, parsec, pretty, stdenv }:
            mkDerivation {
              pname = "language-dot";
              version = "0.1.0";
              sha256 = "108m1dax4s286dr40dy9qxk6r6gpiwjx7646v4lx3vs51h08yh8m";
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
        tf-random = callPackage
          (
            { mkDerivation, base, primitive, random, stdenv, time }:
            mkDerivation {
              pname = "tf-random";
              version = "0.5";
              sha256 = "0445r2nns6009fmq0xbfpyv7jpzwv0snccjdg7hwj4xk4z0cwc1f";
              libraryHaskellDepends = [ base primitive random time ];
              description = "High-quality splittable pseudorandom number generator";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit primitive random; };
        QuickCheck = callPackage
          (
            { mkDerivation, base, containers, random, stdenv, template-haskell
            , test-framework, tf-random, transformers
            }:
            mkDerivation {
              pname = "QuickCheck";
              version = "2.9.1";
              sha256 = "0pyhakj0k43m1a42a5173ss1yv3yaym4ymdcs6dpz9lmz2z9qxpq";
              libraryHaskellDepends = [
                base containers random template-haskell tf-random transformers
              ];
              testHaskellDepends = [
                base containers template-haskell test-framework
              ];
              homepage = "https://github.com/nick8325/quickcheck";
              description = "Automatic testing of Haskell programs";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit random tf-random; };
        quickcheck-io = callPackage
          (
            { mkDerivation, base, HUnit, QuickCheck, stdenv }:
            mkDerivation {
              pname = "quickcheck-io";
              version = "0.1.3";
              sha256 = "1d68fcb9cx1bk8yzq28d4hbwjwj4y5y0kldd1nxlq7n54r75i66p";
              libraryHaskellDepends = [ base HUnit QuickCheck ];
              homepage = "https://github.com/hspec/quickcheck-io#readme";
              description = "Use HUnit assertions as QuickCheck properties";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit QuickCheck; };
        hspec-core = callPackage
          (
            { mkDerivation, ansi-terminal, async, base, deepseq
            , hspec-expectations, hspec-meta, HUnit, process, QuickCheck
            , quickcheck-io, random, setenv, silently, stdenv, tf-random, time
            , transformers
            }:
            mkDerivation {
              pname = "hspec-core";
              version = "2.2.3";
              sha256 = "0llnr7gg1xa1l8jz9ivhjq7q12773x2i5xp3wlyyvq0sj9cnkyh1";
              libraryHaskellDepends = [
                ansi-terminal async base deepseq hspec-expectations HUnit
                QuickCheck quickcheck-io random setenv tf-random time transformers
              ];
              testHaskellDepends = [
                ansi-terminal async base deepseq hspec-expectations hspec-meta
                HUnit process QuickCheck quickcheck-io random setenv silently
                tf-random time transformers
              ];
              homepage = "http://hspec.github.io/";
              description = "A Testing Framework for Haskell";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit ansi-terminal async hspec-expectations HUnit QuickCheck quickcheck-io random setenv tf-random; };
        hspec = callPackage
          (
            { mkDerivation, base, directory, hspec-core, hspec-discover
            , hspec-expectations, hspec-meta, HUnit, QuickCheck, stdenv
            , stringbuilder, transformers
            }:
            mkDerivation {
              pname = "hspec";
              version = "2.2.3";
              sha256 = "0432dxkxrmsvz78g8inwhklid677161yp8r0pw8cd1bdx179j7ji";
              libraryHaskellDepends = [
                base hspec-core hspec-discover hspec-expectations HUnit QuickCheck
                transformers
              ];
              testHaskellDepends = [
                base directory hspec-core hspec-meta stringbuilder
              ];
              homepage = "http://hspec.github.io/";
              description = "A Testing Framework for Haskell";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hspec-core hspec-discover hspec-expectations HUnit QuickCheck; };
        transformers-compat = callPackage
          (
            { mkDerivation, base, ghc-prim, stdenv, transformers }:
            mkDerivation {
              pname = "transformers-compat";
              version = "0.5.1.4";
              sha256 = "17yam0199fh9ndsn9n69jx9nvbsmymzzwbi23dck3dk4q57fz0fq";
              libraryHaskellDepends = [ base ghc-prim transformers ];
              homepage = "http://github.com/ekmett/transformers-compat/";
              description = "A small compatibility shim exposing the new types from transformers 0.3 and 0.4 to older Haskell platforms.";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        Glob = callPackage
          (
            { mkDerivation, base, containers, directory, dlist, filepath, HUnit
            , QuickCheck, stdenv, test-framework, test-framework-hunit
            , test-framework-quickcheck2, transformers, transformers-compat
            }:
            mkDerivation {
              pname = "Glob";
              version = "0.7.11";
              sha256 = "0xy332m8wbr3cgvayqvhq7rli3yd3k71cdzqzsg159z6w05zj8qa";
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
        exceptions = callPackage
          (
            { mkDerivation, base, mtl, QuickCheck, stdenv, stm
            , template-haskell, test-framework, test-framework-quickcheck2
            , transformers, transformers-compat
            }:
            mkDerivation {
              pname = "exceptions";
              version = "0.8.3";
              sha256 = "1gl7xzffsqmigam6zg0jsglncgzxqafld2p6kb7ccp9xirzdjsjd";
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
        mmorph = callPackage
          (
            { mkDerivation, base, mtl, stdenv, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "mmorph";
              version = "1.0.6";
              sha256 = "1i8dzrc5qi3ryc9vrrmpn3sihmramsbhhd592w4w2k5g26qr3hql";
              libraryHaskellDepends = [
                base mtl transformers transformers-compat
              ];
              description = "Monad morphisms";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit mtl transformers-compat; };
        tagged = callPackage
          (
            { mkDerivation, base, deepseq, stdenv, template-haskell
            , transformers, transformers-compat
            }:
            mkDerivation {
              pname = "tagged";
              version = "0.8.5";
              sha256 = "16cdzh0bw16nvjnyyy5j9s60malhz4nnazw96vxb0xzdap4m2z74";
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
        temporary = callPackage
          (
            { mkDerivation, base, directory, exceptions, filepath, stdenv
            , transformers, unix
            }:
            mkDerivation {
              pname = "temporary";
              version = "1.2.0.4";
              sha256 = "0qk741yqnpd69sksgks2vb7zi50rglp9m498lzw4sh268a017rsi";
              libraryHaskellDepends = [
                base directory exceptions filepath transformers unix
              ];
              homepage = "http://www.github.com/feuerbach/temporary";
              description = "Portable temporary file and directory support for Windows and Unix, based on code from Cabal";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit exceptions; };
        mockery = callPackage
          (
            { mkDerivation, base, base-compat, bytestring, directory, filepath
            , hspec, logging-facade, stdenv, temporary
            }:
            mkDerivation {
              pname = "mockery";
              version = "0.3.3";
              sha256 = "1m7sq2vclgir3qbpngzl3g87ks4034blwwf7p3h02c0jlcwpl5b1";
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
        transformers-base = callPackage
          (
            { mkDerivation, base, stdenv, stm, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "transformers-base";
              version = "0.4.4";
              sha256 = "11r3slgpgpra6zi2kjg3g60gvv17b1fh6qxipcpk8n86qx7lk8va";
              revision = "1";
              editedCabalFile = "fb1a305f29cbf6ac182af7e67efaae9fcb9664d8d9606bb8a7f3414ad4c8d7a4";
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
        monad-control = callPackage
          (
            { mkDerivation, base, stdenv, stm, transformers, transformers-base
            , transformers-compat
            }:
            mkDerivation {
              pname = "monad-control";
              version = "1.0.1.0";
              sha256 = "1x018gi5irznx5rgzmkr2nrgh26r8cvqwkcfc6n6y05pdjf21c6l";
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
        lifted-base = callPackage
          (
            { mkDerivation, base, HUnit, monad-control, stdenv, test-framework
            , test-framework-hunit, transformers, transformers-base
            , transformers-compat
            }:
            mkDerivation {
              pname = "lifted-base";
              version = "0.2.3.8";
              sha256 = "17yz4n7q96x4cp8vxai8csn2vmpigxvipkfh48arahf91f0xy18n";
              libraryHaskellDepends = [ base monad-control transformers-base ];
              testHaskellDepends = [
                base HUnit monad-control test-framework test-framework-hunit
                transformers transformers-base transformers-compat
              ];
              homepage = "https://github.com/basvandijk/lifted-base";
              description = "lifted IO operations from the base library";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit monad-control transformers-base transformers-compat; };
        enclosed-exceptions = callPackage
          (
            { mkDerivation, async, base, deepseq, hspec, lifted-base
            , monad-control, QuickCheck, stdenv, stm, transformers
            , transformers-base
            }:
            mkDerivation {
              pname = "enclosed-exceptions";
              version = "1.0.2";
              sha256 = "1wc9h6zdnb5impvvml6vnjapajjanw7zgpnzg7c0v7115nwfm6vv";
              revision = "1";
              editedCabalFile = "40b6f9bc9de19819e54b215008a8b60862f2558119dc49e7c747a5bac4435566";
              libraryHaskellDepends = [
                base deepseq lifted-base monad-control transformers
                transformers-base
              ];
              testHaskellDepends = [
                async base deepseq hspec lifted-base monad-control QuickCheck stm
                transformers transformers-base
              ];
              homepage = "https://github.com/jcristovao/enclosed-exceptions";
              description = "Catching all exceptions from within an enclosed computation";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit async hspec lifted-base monad-control QuickCheck stm transformers-base; };
        resourcet = callPackage
          (
            { mkDerivation, base, containers, exceptions, hspec, lifted-base
            , mmorph, monad-control, mtl, stdenv, transformers
            , transformers-base, transformers-compat
            }:
            mkDerivation {
              pname = "resourcet";
              version = "1.1.7.5";
              sha256 = "0nj0gwfd05divpdn7m47gy6bpcrwn3zk81gc303k0smrbqi0xlq5";
              libraryHaskellDepends = [
                base containers exceptions lifted-base mmorph monad-control mtl
                transformers transformers-base transformers-compat
              ];
              testHaskellDepends = [ base hspec lifted-base transformers ];
              homepage = "http://github.com/snoyberg/conduit";
              description = "Deterministic allocation and freeing of scarce resources";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit exceptions hspec lifted-base mmorph monad-control mtl transformers-base transformers-compat; };
        conduit = callPackage
          (
            { mkDerivation, base, containers, exceptions, hspec, lifted-base
            , mmorph, mtl, QuickCheck, resourcet, safe, stdenv, transformers
            , transformers-base
            }:
            mkDerivation {
              pname = "conduit";
              version = "1.2.7";
              sha256 = "1r9vxpbcy441niw80byg68gkkn2xrqbz8l6x1q4d70fgassivcin";
              libraryHaskellDepends = [
                base exceptions lifted-base mmorph mtl resourcet transformers
                transformers-base
              ];
              testHaskellDepends = [
                base containers exceptions hspec mtl QuickCheck resourcet safe
                transformers
              ];
              homepage = "http://github.com/snoyberg/conduit";
              description = "Streaming data processing library";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit exceptions hspec lifted-base mmorph mtl QuickCheck resourcet safe transformers-base; };
        unix-compat = callPackage
          (
            { mkDerivation, base, stdenv, unix }:
            mkDerivation {
              pname = "unix-compat";
              version = "0.4.2.0";
              sha256 = "036nv05w0yjxc3rfpar60ddjrlzc40mdgr5k6ihvwlvqfmq1gw9m";
              libraryHaskellDepends = [ base unix ];
              homepage = "http://github.com/jystic/unix-compat";
              description = "Portable POSIX-compatibility layer";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        unordered-containers = callPackage
          (
            { mkDerivation, base, ChasingBottoms, containers, deepseq, hashable
            , HUnit, QuickCheck, stdenv, test-framework, test-framework-hunit
            , test-framework-quickcheck2
            }:
            mkDerivation {
              pname = "unordered-containers";
              version = "0.2.7.1";
              sha256 = "00npqiphivjp2d7ryqsdavfn4m5v3w1lq2azhdsrfh0wsvqpg4ig";
              libraryHaskellDepends = [ base deepseq hashable ];
              testHaskellDepends = [
                base ChasingBottoms containers hashable HUnit QuickCheck
                test-framework test-framework-hunit test-framework-quickcheck2
              ];
              homepage = "https://github.com/tibbe/unordered-containers";
              description = "Efficient hashing-based container types";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hashable HUnit QuickCheck; };
        vector = callPackage
          (
            { mkDerivation, base, deepseq, ghc-prim, primitive, QuickCheck
            , random, stdenv, template-haskell, test-framework
            , test-framework-quickcheck2, transformers
            }:
            mkDerivation {
              pname = "vector";
              version = "0.11.0.0";
              sha256 = "1r1jlksy7b0kb0fy00g64isk6nyd9wzzdq31gx5v1wn38knj0lqa";
              revision = "1";
              editedCabalFile = "dfdf3252519ff35da59f977b7d37d6c5a6660673ce1234899af0111f7ece9c66";
              libraryHaskellDepends = [ base deepseq ghc-prim primitive ];
              testHaskellDepends = [
                base QuickCheck random template-haskell test-framework
                test-framework-quickcheck2 transformers
              ];
              homepage = "https://github.com/haskell/vector";
              description = "Efficient Arrays";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit primitive QuickCheck random; };
        scientific = callPackage
          (
            { mkDerivation, base, binary, bytestring, containers, deepseq
            , ghc-prim, hashable, integer-gmp, QuickCheck, smallcheck, stdenv
            , tasty, tasty-ant-xml, tasty-hunit, tasty-quickcheck
            , tasty-smallcheck, text, vector
            }:
            mkDerivation {
              pname = "scientific";
              version = "0.3.4.9";
              sha256 = "1a0q15kq0pk3pabxh536wgphh8713hhn8n55gm6s1y8a5dk310qh";
              libraryHaskellDepends = [
                base binary bytestring containers deepseq ghc-prim hashable
                integer-gmp text vector
              ];
              testHaskellDepends = [
                base binary bytestring QuickCheck smallcheck tasty tasty-ant-xml
                tasty-hunit tasty-quickcheck tasty-smallcheck text
              ];
              homepage = "https://github.com/basvandijk/scientific";
              description = "Numbers represented using scientific notation";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hashable QuickCheck text vector; };
        attoparsec = callPackage
          (
            { mkDerivation, array, base, bytestring, containers, deepseq
            , QuickCheck, quickcheck-unicode, scientific, stdenv, tasty
            , tasty-quickcheck, text, transformers, vector
            }:
            mkDerivation {
              pname = "attoparsec";
              version = "0.13.0.2";
              sha256 = "0spcybahmqxnmngfa9cf5rh7n2r8njrgkgwb6iplmfj4ys0z7xv9";
              libraryHaskellDepends = [
                array base bytestring containers deepseq scientific text
                transformers
              ];
              testHaskellDepends = [
                array base bytestring deepseq QuickCheck quickcheck-unicode
                scientific tasty tasty-quickcheck text transformers vector
              ];
              homepage = "https://github.com/bos/attoparsec";
              description = "Fast combinator parsing for bytestrings and text";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit QuickCheck scientific text vector; };
        aeson = callPackage
          (
            { mkDerivation, attoparsec, base, base-orphans, bytestring
            , containers, deepseq, dlist, fail, ghc-prim, hashable, HUnit, mtl
            , QuickCheck, quickcheck-instances, scientific, stdenv, syb, tagged
            , template-haskell, test-framework, test-framework-hunit
            , test-framework-quickcheck2, text, time, transformers
            , unordered-containers, vector
            }:
            mkDerivation {
              pname = "aeson";
              version = "0.11.2.1";
              sha256 = "0k5p06pik7iyjm1jjkjbpqqn0mqps6b8mz9p9sp9hmganl4cffyc";
              libraryHaskellDepends = [
                attoparsec base bytestring containers deepseq dlist fail ghc-prim
                hashable mtl scientific syb tagged template-haskell text time
                transformers unordered-containers vector
              ];
              testHaskellDepends = [
                attoparsec base base-orphans bytestring containers ghc-prim
                hashable HUnit QuickCheck quickcheck-instances tagged
                template-haskell test-framework test-framework-hunit
                test-framework-quickcheck2 text time unordered-containers vector
              ];
              homepage = "https://github.com/bos/aeson";
              description = "Fast JSON parsing and encoding";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit attoparsec dlist fail hashable HUnit mtl QuickCheck scientific syb tagged text unordered-containers vector; };
        with-location = callPackage
          (
            { mkDerivation, base, hspec, stdenv }:
            mkDerivation {
              pname = "with-location";
              version = "0.1.0";
              sha256 = "1rzxvsyh8x3ql3zh7gyw9hjx9bl4v73h0y5kzgaxcfcdn86dg49c";
              libraryHaskellDepends = [ base ];
              testHaskellDepends = [ base hspec ];
              homepage = "https://github.com/sol/with-location#readme";
              description = "Use ImplicitParams-based source locations in a backward compatible way";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hspec; };
        yaml = callPackage
          (
            { mkDerivation, aeson, aeson-qq, attoparsec, base, base-compat
            , bytestring, conduit, containers, directory, enclosed-exceptions
            , filepath, hspec, HUnit, mockery, resourcet, scientific
            , semigroups, stdenv, text, transformers, unordered-containers
            , vector
            }:
            mkDerivation {
              pname = "yaml";
              version = "0.8.18.1";
              sha256 = "0ymcr4y86i74my6agmp5pql7wzqr6va7dl13f4qdfg94kn3hwq94";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [
                aeson attoparsec base bytestring conduit containers directory
                enclosed-exceptions filepath resourcet scientific semigroups text
                transformers unordered-containers vector
              ];
              executableHaskellDepends = [ aeson base bytestring ];
              testHaskellDepends = [
                aeson aeson-qq base base-compat bytestring conduit hspec HUnit
                mockery resourcet text transformers unordered-containers vector
              ];
              homepage = "http://github.com/snoyberg/yaml/";
              description = "Support for parsing and rendering YAML documents";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit aeson attoparsec base-compat conduit enclosed-exceptions hspec HUnit mockery resourcet scientific semigroups text unordered-containers vector; };
        hpack = callPackage
          (
            { mkDerivation, aeson, aeson-qq, base, base-compat, containers
            , deepseq, directory, filepath, Glob, hspec, interpolate, mockery
            , QuickCheck, stdenv, temporary, text, unordered-containers, yaml
            }:
            mkDerivation {
              pname = "hpack";
              version = "0.14.1";
              sha256 = "100jqn5y6j2b6dsq1kln89kbzpz3a8rg74mbxwk2ix2jkiqyhc59";
              revision = "1";
              editedCabalFile = "59a63c997869623189c5e2bb3df8b1da09dda3a2258cbef43a87cbb4a40addc5";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [
                aeson base base-compat containers deepseq directory filepath Glob
                text unordered-containers yaml
              ];
              executableHaskellDepends = [
                aeson base base-compat containers deepseq directory filepath Glob
                text unordered-containers yaml
              ];
              testHaskellDepends = [
                aeson aeson-qq base base-compat containers deepseq directory
                filepath Glob hspec interpolate mockery QuickCheck temporary text
                unordered-containers yaml
              ];
              homepage = "https://github.com/sol/hpack#readme";
              description = "An alternative format for Haskell packages";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit aeson base-compat Glob hspec mockery QuickCheck temporary text unordered-containers yaml; };
      };

      newResolver = compiler.override {
        overrides = overrideFunction;
      };

    in newResolver;
}
