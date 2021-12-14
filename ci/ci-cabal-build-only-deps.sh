#!/bin/bash
# Build haskell:tag with only dependencies
# set -e

FLAGS=(--only-dependencies)

echo "$1"
if [[ $1 == "musl-haskell" ]]
then
FLAGS+=(--constraint='text +integer-simple' \
        --constraint='cryptonite -integer-gmp' \
        --enable-executable-static \
        --enable-static )
fi

echo "Running cabal build with following configuration: ${FLAGS[@]}"
cabal build all "${FLAGS[@]}"