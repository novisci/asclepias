#!/bin/bash
set -e

cabal build all \
  -j \
  --enable-tests \
  --enable-benchmarks \

cabal test all \
  -j \
  --test-show-details=always


# If on the main branch then run haddock
if [[ "$CI_COMMIT_BRANCH" == "$CI_DEFAULT_BRANCH" ]]; then
  ./ci/ci-cabal-haddock-docs.sh
fi