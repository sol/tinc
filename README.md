# stack-sandbox

Very experimental!!!

`stack-sandbox` allows you to create a new cabal sandbox that contains all
packages from another sandbox (by registering every package from the parent in
the child). The two sandboxes are somewhat independent: Installing or
unregistering packages in one won't affect the other. Only deleting (or
modifying) the actual package files (somewhere in directory of the parent
sandbox) will destroy the child sandbox.
