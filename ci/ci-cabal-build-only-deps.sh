#!/bin/sh
# Build haskell:tag with only dependencies
set -e

FLAGS=(--only-dependencies)

if [[$1 == "musl-haskell"]]
then
FLAGS+=(--constraint='text +integer-simple' \
        --constraint='cryptonite -integer-gmp' \
        --enable-executable-static \
        --enable-static )
fi

cabal build all "${FLAGS[@]}"