# tinc: A package manager for Haskell

**Very experimental!!!**

`tinc` installs dependencies for a project that you work on into a sandbox.
While doing so it caches installed packages (in `~/.tinc/cache`).  When a
package with the same transitive dependencies is already in the cache, it
reuses that cached package instead of building it again.
