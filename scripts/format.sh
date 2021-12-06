#!/bin/sh

find . -name "*.hs" -not -path './dist-newstyle/*' -exec sh -c '
  brittany {} --write-mode=inplace' ';'