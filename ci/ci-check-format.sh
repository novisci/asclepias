#!/bin/sh
# Checks that the source code is formatted using the stylish-haskell
# formatter (https://hackage.haskell.org/package/stylish-haskell)
set -e 

# find . -name "*.hs" -not -path './dist-newstyle/*' -print0 |
#   xargs -0 stylish-haskell --config ci/ci-stylish-haskell.yaml


if find . -name "*.hs" -not -path './dist-newstyle/*' -print0 |
  xargs -0 stylish-haskell --config ci/ci-stylish-haskell.yaml 2>&1 |
  grep '^'; then
echo ""
echo "formatting changes required. Run ./scripts/format.sh"
exit 1
else 
exit 0
fi