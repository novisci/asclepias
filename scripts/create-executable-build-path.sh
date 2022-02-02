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

# Ensure that we've found exactly 1 executable path. This assumes that there
# aren't any newlines in any of the paths, otherwise we will get spurious
# results
n_paths=$(printf "%s" "$paths" | grep -c "^")
if [ "$n_paths" -eq 0 ]; then
    1>&2 echo "error: couldn't find a path for the executable with package name $PKG and component $COMPONENT"
    exit 1
elif [ "$n_paths" -ge 2 ]; then
    1>&2 echo 'error: found more than one path:'
    1>&2 echo "$paths"
    exit 1
fi

# Print the path to the executable
echo "$paths"
