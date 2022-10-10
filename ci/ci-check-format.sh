#!/bin/sh
# Checks that the source code is formatted using the stylish-haskell
# formatter (https://hackage.haskell.org/package/stylish-haskell)
set -e 

stylish-haskell --version

# For Taggers exclusion, see
# https://gitlab.com/TargetRWE/epistats/nsstat/asclepias/-/issues/333
find . -name "*.hs" -not -path '*/Taggers.hs' -not -path './dist-newstyle/*' -print0 |
  xargs -0 stylish-haskell --config ci/ci-stylish-haskell.yaml
