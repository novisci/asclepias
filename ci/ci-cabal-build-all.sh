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
