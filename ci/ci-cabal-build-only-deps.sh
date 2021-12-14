#!/bin/bash
# Build hasklepias project with --only-dependencies to speed later builds.
set -e

FLAGS=(--only-dependencies)

echo "Hey: $1"

if [[ $1 == "musl-haskell" ]]
then
FLAGS+=(--constraint='text +integer-simple' \
        --constraint='cryptonite -integer-gmp' \
        --enable-executable-static \
        --enable-static )
fi

echo "Running cabal build with following configuration: ${FLAGS[@]}"

cabal build all "${FLAGS[@]}"