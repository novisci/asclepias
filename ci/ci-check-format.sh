#!/bin/sh
# Checks that the source code is formatted using the stylish-haskell
# formatter (https://hackage.haskell.org/package/stylish-haskell)
set -e 

stylish-haskell --version

find . -name "*.hs" -not -path './dist-newstyle/*' -print0 |
  xargs -0 stylish-haskell --recursive --config ci/ci-stylish-haskell.yaml
