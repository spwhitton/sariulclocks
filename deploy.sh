#!/bin/sh

# simple deploy script for demeter

cabal build
cp -RL assets/* dist/build/sariulclocks.cgi/sariulclocks.cgi $HOME/html/
# TODO: run strip on binary
cp -RL ../schoolclock/sounds $HOME/html
