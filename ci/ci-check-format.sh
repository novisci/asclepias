#!/bin/sh
# Checks that the source code is formatted using the stylish-haskell
# formatter (https://hackage.haskell.org/package/stylish-haskell)
set -e 

# stylish-haskell --version

# find . -name "*.hs" -not -path './dist-newstyle/*' -print0 |
#   xargs -0 stylish-haskell --recursive --config ci/ci-stylish-haskell.yaml


curl -sL https://raw.github.com/haskell/stylish-haskell/master/scripts/latest.sh | sh -s . 

# --recursive --config ci/ci-stylish-haskell.yaml 
