#!/bin/sh
# Checks that the source code is formatted using the brittany
# formatter (https://hackage.haskell.org/package/brittany)
set -e 

find . -name "*.hs" -not -path './dist-newstyle/*' |
  xargs brittany -c