#!/bin/sh
#
# Creates the path to a built executable for cabal >= 2.2 according to this document:
# https://cabal.readthedocs.io/en/3.6/nix-local-build.html?highlight=path%20to%20executable#where-are-my-build-products

GHCV=$(ghc --version | sed 's/[[:alpha:]]*[[:space:]]*//g' | sed 's/,//g')
SYS=$(uname -s | tr '[:upper:]' '[:lower:]')
PKG=$1
PKGV=$2
COMPONENT=$3

echo "dist-newstyle/build/$(uname -m)-$SYS/ghc-$GHCV/$PKG-$PKGV/x/$COMPONENT/build/$COMPONENT/$COMPONENT"
