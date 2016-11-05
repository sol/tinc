# tinc: A dependency manager for Haskell

`tinc` installs dependencies for a project that you work on into a sandbox.
While doing so it caches installed packages (in `~/.tinc/cache`).  When a
package with the same transitive dependencies is already in the cache, it
reuses that cached package instead of building it again.

`tinc` uses an exact algorithm for determining reusability.  This guarantees
100% cache reuse.  A package with the same transitive dependencies is never
built twice.

`tinc` does not take any cached packages into account when resolving
dependencies.  Running

    $ tinc

gives you the exact same result as

    $ cabal sandbox delete
    $ cabal sandbox init
    $ cabal install --only-dependencies --enable-tests

`tinc` is idempotent.  It's safe to run `tinc` multiple times.  Running `tinc`
after changing the `.cabal`-file of a project or after running `cabal update`
results in a new updated sandbox.

If `tinc` fails / terminates for some reason, it does not modify anything
(neither the cache nor any existing sandboxes).  Interrupting a running `tinc`
build is always safe.

## Optionally use Nix for package caching
By default, `tinc` maintains its own package cache under `~/.tinc/cache`.

However, it can optionally use nixpkgs for package caching.
If `tinc` is installed somewhere under `/nix`, this is the default.

To change the default, you can set the environment variable
`TINC_USE_NIX` to either `yes` or `no`, specifically:
```bash
export TINC_USE_NIX=yes  # use nix, even if not installed under /nix
export TINC_USE_NIX=no   # do not use nix, even if installed under /nix
```

More information can be found in [NIX.md](NIX.md).
