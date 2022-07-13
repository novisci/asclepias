#!/bin/sh

cabal build hasklepias-main --enable-benchmarks

/Users/bradley.saul/Documents/novisci/software/asclepias/dist-newstyle/build/aarch64-osx/ghc-8.10.7/hasklepias-main-0.26.1/b/hasklepias/build/hasklepias/hasklepias --csv benchmarks.csv +RTS -T

cabal install hasklepias-main


# These take awhile...
# hyperfine --warmup 1 \
#     --min-runs=3 \
#     --max-runs=3 \
#     --parameter-list app testA,testC,testE \
#     '{app} --dir hasklepias-main/lineFilter-test --file 10groups-100000lines.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 100000groups-10lines.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 10groups-100000lines-allfail.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 100000groups-10lines-allfail.jsonl'

# hyperfine --warmup 3 \
#     --parameter-list app testA,testC,testE \
#     '{app} --dir hasklepias-main/lineFilter-test --file 100groups-10000lines.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 1000groups-1000lines.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 10000groups-100lines.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 100groups-10000lines-allfail.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 1000groups-1000lines-allfail.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 10000groups-100lines-allfail.jsonl' \

