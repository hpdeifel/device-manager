## Installation

### The `curl | sh` approach

Install [`stack`](https://github.com/commercialhaskell/stack) and
clone this repository:

    git clone https://github.com/rootzlevel/device-manager.git

The following system-libraries have to be installed:

 - libxml2 (possibly in the -dev version)

Use stack to download all Haskell dependencies, build the code and
install the binaries to `~/.local/bin`:

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
[patched version of the haskell-dbus library](https://gitlab.com/hpdeifel/haskell-dbus/tree/path_namespace).
Then run the following commands in the toplevel directory:

    runhaskell Setup.hs configure --bindir ~/whatever
	runhaskell Setup.hs build
	runhaskell Setup.hs install

## Usage

There are two independent executables:

### devman

The actual terminal UI. Its usage should be pretty straightforward.

### devnotify

A small programm that displays desktop notifications for various
events such as plugging in or removing devices.
