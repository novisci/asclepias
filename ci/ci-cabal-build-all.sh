#!/bin/bash
set -e

cabal build all \
  -j \
  --enable-tests \
  --enable-benchmarks \
  # --test-show-details=always # only seems to work using cabal test
