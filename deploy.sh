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

cd $HOME/src/sariulclocks
mkdir -p $dest

# 2. build our software

if ! [ -d "$HOME/.cabal/config" ]; then
    mkcabalc
fi
if ! [ -d "$HOME/.cabal/packages" ]; then
    cabal update
fi
if ! [ -d ".cabal-sandbox" ]; then
    cabal sandbox init
fi
cabal install --only-dependencies
cabal configure
cabal build

# 3. copy our software into place

cp -RL assets/* assets/.htaccess dist/build/sariulclocks.cgi/sariulclocks.cgi $dest
# TODO: run strip on binary
cp -RL ../schoolclock/sounds $dest
if [ "$1" = "live" ]; then
    mkdir -p $HOME/local/bin
    cp dist/build/sariulccron/sariulccron $HOME/local/bin
fi

# 4. further permissions & dirs

cd $HOME
# mkhomepg -p
mkdir -p $dest/data
if ! [ -e "$dest/password" ]; then
    echo "dummy_password" > $dest/password
    echo "Please update password in $dest/password!"
fi
chmod 777 $dest/data
chmod 755 $dest/sariulclocks.cgi
