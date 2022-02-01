#!/bin/bash
set -e

# cabal update

# cabal build all \
#   -j \
#   --enable-tests \
#   --enable-benchmarks

# cabal test all \
#   -j \
#   --test-show-details=always

# # Report test coverage. NOTE: would need to add `--enable-coverage` to the above
# # commands before uncommenting the following lines.
# bash ci/ci-coverage.sh > coverage-report.txt
# cat coverage-report.txt

# If on the main branch then run Haddock. The Haddock directory is listed as an
# artifact so we need to ensure it's creation unconditionally. If being run as
# part of the CI then require the presence of $HADDOCK_DIR, otherwise a fallback
# of `"install"` is provided as a convenience for local testing
[[ -n $GITLAB_CI ]] && [[ -z $HADDOCK_DIR ]] && exit 1
mkdir -p ${HADDOCK_DIR:-install}
if [[ "$CI_COMMIT_BRANCH" == "$CI_DEFAULT_BRANCH" ]]; then
  ./ci/ci-cabal-haddock-docs.sh
fi
