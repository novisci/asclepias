#!/bin/sh
# Formats haskell source code files in place using brittany source code formatter.

find . -name "*.hs" -not -path './dist-newstyle/*' -exec sh -c '
  brittany {} --write-mode=inplace' ';'