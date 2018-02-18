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

If you want to install `tinc` with `nix-env` you can do so by running
```
nix-env -i -f default.nix
```
inside this repository.

## Enabling plug-ins
`tinc` comes with a number of useful plug-ins. To enable them symlink `plugins` to `~/.tinc/plugins`:

```
mkdir -p ~/.tinc/ && ln -s `pwd`/plugins/ ~/.tinc/
```

# Additional dependencies and dependencies from GitHub

You can specify additional dependencies in a file named `tinc.yaml`.  These
dependencies may be from a variety of sources:

```yaml
dependencies:
  # additional dependency from Hackage
  - foo

  # override version constraint in package.yaml
  - foo == 0.1.0

  # dependency from GitHub
  - name: foo
    github: owner/repo
    ref: master
    subdir: some-dir # optional subdirectory

  # dependency from arbitrary Git repositories
  - name: foo
    git: http://...
    ref: master

  # local dependency (will use cabal sdist)
  - name: foo
    path: ../foo/
```

(Note: The accepted syntax for dependencies is the same as what is accepted in
`package.yaml`.)

Dependencies from `tinc.yaml` will be added to your sandbox or can override
existing dependencies from `package.yaml`.

You can also use `tinc.yaml` without any `package.yaml` file (or `.cabal` file)
to create a sandbox with dependencies.
