#!/bin/sh
#
# Creates the path to a built executable for cabal >= 2.2 according to this document:
# https://cabal.readthedocs.io/en/3.6/nix-local-build.html#where-are-my-build-products

PKG=$1
# PKGV=$2
COMPONENT=$3

# Ensure we have at least three positional arguments
if [ -z "$3" ]; then
    1>&2 echo 'error: expects 3 positional arguments'
    exit 1
fi

# # The following commmand does not appear to consistently give correct results,
# # so for the time-being it is commented out in favor of a `find`-based
# # alternative
# GHCV=$(ghc --version | sed 's/[[:alpha:]]*[[:space:]]*//g' | sed 's/,//g')
# SYS=$(uname -s | tr '[:upper:]' '[:lower:]')
# echo "dist-newstyle/build/$(uname -m)-$SYS/ghc-$GHCV/$PKG-$PKGV/x/$COMPONENT/build/$COMPONENT/$COMPONENT"

# Find the path to the executable
paths=$(find dist-newstyle -type f -executable -path "*$PKG*$COMPONENT" -print)
scripts/assert-exactly-one-line.sh "$paths"

# Print the path to the executable
echo "$paths"
