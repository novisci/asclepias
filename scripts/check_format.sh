#!/bin/sh
# Checks that the source code is formatted
set -e 

find . -name "*.hs" -not -path './dist-newstyle/*' |
  xargs brittany -c