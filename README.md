## Installation

### The `curl | sh` approach

Install [`stack`](https://github.com/commercialhaskell/stack) and
clone the this repository:

    git clone -b brick https://github.com/rootzlevel/device-manager.git

Use stack to download all dependencies, build the code and install the
binaries to `~/.local/bin`:

    cd device-manager/
	stack build --copy-bins

You can specify another destination directory:

    stack --local-bin-path ~/somewhere build --copy-bins

Stack will likely ask you to install a specific version of the GHC
haskell compiler. You can use your own version with `--skip-ghc-check`
or install it locally with `stack setup`.


### The manual way

Install GHC and all the dependencies listed in the `.cabal` file.
Download and install the
[patched version of the haskell-dbus](https://gitlab.com/hpdeifel/haskell-dbus/tree/path_namespace).
Then run the following commands in the toplevel directory:

    runhaskell Setup.hs configure --bindir ~/whatever
	runhaskell Setup.hs build
	runhaskell Setup.hs install
