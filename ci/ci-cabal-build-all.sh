#!/bin/bash
set -e

cabal update

cabal build all \
  -j \
  --enable-tests \
  --enable-benchmarks

cabal test all \
  -j \
  --test-show-details=always

# # Report test coverage. NOTE: would need to add `--enable-coverage` to the above
# # commands before uncommenting the following lines.
# bash ci/ci-coverage.sh > coverage-report.txt
# cat coverage-report.txt

# If on the main branch then run haddock
if [[ "$CI_COMMIT_BRANCH" == "$CI_DEFAULT_BRANCH" ]]; then
  ./ci/ci-cabal-haddock-docs.sh
fi
