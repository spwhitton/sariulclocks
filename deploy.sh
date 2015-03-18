#!/bin/bash
. $HOME/.shenv

# simple deploy script: first make sure GHC and cabal are available
# (on the MetaArray, see comproc.org "MetaArray Haskell")

# 1. choose where to deploy

live="$HOME/html/sariul/cgi-bin"
devel="$HOME/html/sariul-devel/cgi-bin"

if [ "$1" = "live" ]; then
    dest="$live"
else
    if [ "$1" = "devel" ]; then
        dest="$devel"
    else
        echo "Please supply one of \`devel', \`live' cmd line args" >&2
        exit 1
    fi
fi

cd ~/src/sariulclocks
mkdir -p $dest

# 2. build our software

if ! [ -d ".cabal-sandbox" ]; then
    cabal sandbox init
fi
cabal install --only-dependencies
cabal configure
cabal build
# TODO: make this build actually work on ma!  Missing librt.  In the
# meantime, manually copy sariulclocks.cgi into place from demeter.

# 3. copy our software into place

cp -RL assets/* assets/.htaccess dist/build/sariulclocks.cgi/sariulclocks.cgi $dest
# TODO: run strip on binary
cp -RL ../schoolclock/sounds $dest

# 4. further permissions & dirs

cd $HOME
# mkhomepg -p
mkdir -p $dest/data
chmod 777 $dest/data
chmod 755 $dest/sariulclocks.cgi

# TODO: also put script that resets clocks and deducts points at the
# end of the week into ~/local/bin ready to be cronned
