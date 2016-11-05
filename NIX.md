<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [`tinc` in Nix mode](#tinc-in-nix-mode)
    - [Walkthrough](#walkthrough)
    - [Missing dependencies](#missing-dependencies)
    - [Plugins](#plugins)
    - [Overriding attributes in `default.nix`](#overriding-attributes-in-defaultnix)

<!-- markdown-toc end -->
# `tinc` in Nix mode

When `tinc` is installed under `/nix` or the environment variable `TINC_USE_NIX` is set to `yes`, it operates in Nix mode. Everything is installed under `/nix` including:
  - System dependencies for GHC
  - GHC
  - System and Haskell dependencies for your project (cabal packages)
  - Your project

Essentially, `tinc` does the following:
  1. Use `cabal` to generate an install plan from your `.cabal` file.
  1. Use `cabal2nix` to generate Nix derivations for all dependencies in the install plan. These Nix derivations are stored in `$HOME/.tinc/cache/nix`.
  1. Use `cabal2nix` to generate Nix derivation from the project `.cabal`.
  1. Incorporate the previous Nix derivations and create the final project Nix files, including `default.nix` and `shell.nix`. These files can be used as-is without `tinc` installed.

With the Nix files, you can:
  1. Run `nix-shell` to enter an environment with all the dependencies in scope, which allows you to run `ghci`, `cabal configure`, `cabal build`, `cabal test`, etc.
  1. Run `nix-build` to build your project in `/nix`.


## Walkthrough
It is probably not feasible to follow these steps perfectly because your cabal package index or nixpkgs will not be exactly the same, but the following describes the general steps for using `tinc` to build a sample project:
  1. Imagine it is the end of May 2016. The latest version of QuickCheck is 2.8.2 when we update our cabal package index.
  ```
  cabal update
  ```

  1. We checkout the state of our sample project at this time (any Haskell project should work):
  ```
  git clone https://github.com/robbinch/tinc
  cd tinc
  git reset --hard 405af997c182b89edfc9656612c32616e98c7862
  ```

  1. We pretend we do not have any `.nix` or `tinc.freeze` files:
  ```
  rm -f *.nix tinc.freeze
  ```

  1. Let `tinc` generate the Nix files:
  ```
  export TINC_USE_NIX=yes
  tinc
  ```

  1. We can now use `nix-shell` to configure, build or test the project:
  ```
  nix-shell
  cabal configure --enable-tests
  cabal build
  cabal test
  exit
  ```

  1. As it is common to run `nix-shell` right after `tinc`, the plugin `tinc shell` can be used to do this in one step.

  1. We can also use `nix-build` or `nix-env`:
  ```
  nix-build                         # will build project in /nix
  nix-env -i tinc -f default.nix    # will build and install project in profile
  ```

  1. Instead of directly installing using `nix-env`, the plugin `tinc install` can and should be used. It does the extra steps of packaging using `cabal sdist`, extracting the tarball and resetting the file modification times before running `nix-env`. This creates a consistent derivation (Nix checksum) untainted by extraneous files in the development directory.

  1. It is recommended to keep all `.nix` files and `tinc.freeze` under version control. That way, distributing them with your project gives users the best chance of building your Haskell project with Nix. They can simply do `nix-build` or `nix-env` without having `tinc` installed. They can even use the same commit of `nixpkgs` for better reproducibility.

  1. When you wish to update your project to use the latest cabal packages:
  ```
  cabal update
  rm *.nix tinc.freeze
  tinc shell
  ```

  1. If anything goes wrong, you can revert back to the old `.nix` files to build your project again.


## Missing dependencies
Sometimes, we get missing dependencies. Let's see the problem:
  1. Update to a more recent cabal package index (it's early November 2016 now). The latest version of QuickCheck at this time is 2.9.2.
  ```
  cabal update
  ```

  1. Checkout a more recent project:
  ```
  git clone https://github.com/robbinch/tinc
  cd tinc
  git reset --hard 411d0f319717d01dc71bd5c1faef8035656eaf3d
  ```

  1. Again, we pretend we do not have any `.nix` or `tinc.freeze` files:
  ```
  rm -f *.nix tinc.freeze
  ```

  1. Run `tinc shell` to generate Nix files and enter the build environment:
  ```
  tinc shell
  ```

  1. We get a build error for QuickCheck. It needs the `semigroups` package but our Nix derivation `$HOME/.tinc/cache/nix/QuickCheck-2.9.2.nix` does not have it. This is probably due to `cabal2nix` not working properly with QuickCheck-2.9.2. Add `semigroups` to the list of dependencies:
  ```
  { mkDerivation, base, containers, random, stdenv, template-haskell
  , test-framework, tf-random, transformers
  , semigroups      # <-- add here
  }:
  mkDerivation {
    pname = "QuickCheck";
    version = "2.9.2";
    sha256 = "119np67qvx8hyp9vkg4gr2wv3lj3j6ay2vl4hxspkg43ymb1cp0m";
    libraryHaskellDepends = [
      base containers random template-haskell tf-random transformers
      semigroups    # <-- add here
    ];
    testHaskellDepends = [
      base containers template-haskell test-framework
      semigroups    # <-- add here
    ];
    homepage = "https://github.com/nick8325/quickcheck";
    description = "Automatic testing of Haskell programs";
    license = stdenv.lib.licenses.bsd3;
    doCheck = false;
    doHaddock = false;
  }
  ```

  1. Run `tinc shell` again. This time, we encounter similar problems with `attoparsec`, among others. Fix them the same way in `$HOME/.tinc/cache/nix`.

  1. After all the missing dependencies are added, `tinc shell` will hopefully be successful and we get working Nix derivations again. Put the `.nix` files under version control so we do not need to repeat this in future.


## Plugins
Plugins are simply shell scripts named like `tinc-shell` or `tinc-install` in `$HOME/.tinc/plugins`. They are executed with commands like `tinc shell` or `tinc install`. The standard plugins are provided in the `plugins` directory of the source code and can be copied or symlink-ed to `$HOME/.tinc/plugins`.


## Overriding attributes in `default.nix`
To override attributes (like `doCheck`, `configureFlags`, `postInstall`, etc) in the generated `default.nix`, create the file `default-override.nix`. `tinc` will never overwrite this file but `default.nix` will use it if it exists. See [example](default-override.nix) in `tinc` repository.
