#!/bin/sh
# Formats haskell source code files in place using stylish-haskell source code formatter.

# Check and see if `stylish-haskell` is installed so as to give users a useful message
# if they don't know what stylish-haskell is or how to install it
if ! [ -x "$(command -v stylish-haskell)" ]; then
  echo 'Error: cannot find '\''stylish-haskell'\'' application' >&2
  echo 'Use the command '\''cabal install stylish-haskell'\'' to install the application'  >&2
  exit 1
fi
find . -name "*.hs" -not -path './dist-newstyle/*' -exec sh -c 'stylish-haskell "$1" --i./scnplace' shell {} \;
