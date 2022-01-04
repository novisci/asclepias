#!/bin/bash
set -e

cabal build all \
  -j \
  --enable-tests \
  --enable-benchmarks \
  # --test-show-details=always # only seems to work using cabal test

# If on the main branch then run haddock
# if [[ "$CI_COMMIT_BRANCH" == "$CI_DEFAULT_BRANCH" ]]; then
./ci/ci-cabal-haddock-docs.sh
# fi