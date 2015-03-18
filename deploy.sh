#!/bin/sh

# simple deploy script for demeter

cabal build
cp -RL assets/* dist/build/sariulclocks.cgi/sariulclocks.cgi $HOME/html/
# TODO: run strip on binary
cp -RL ../schoolclock/sounds $HOME/html
mkdir -p $HOME/html/data
chmod 777 $HOME/html/data
# TODO: this script takes "devel" and "live" arguments to deploy live version or a copy for testing new code (into maybe http://spw.sdf.org/sariul-devel/)
