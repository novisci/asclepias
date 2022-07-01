#!/bin/sh

# hyperfine --warmup 3 \
#     --parameter-list app testLineFilterAp-cionduit,testLineFilterApp-fold \
#     '{app} --dir hasklepias-main/lineFilter-test --file 10groups-100000lines.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 100groups-10000lines.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 1000groups-1000lines.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 10000groups-100lines.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 100000groups-10lines.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 10groups-100000lines-allfail.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 100groups-10000lines-allfail.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 1000groups-1000lines-allfail.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 10000groups-100lines-allfail.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 100000groups-10lines-allfail.jsonl' 


# hyperfine --warmup 3 \
#     --parameter-list app testLineFilterApp-conduit,testLineFilterApp-fold \
#     '{app} --dir hasklepias-main/lineFilter-test --file 100groups-10000lines.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 1000groups-1000lines.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 10000groups-100lines.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 100groups-10000lines-allfail.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 1000groups-1000lines-allfail.jsonl' \
#     '{app} --dir hasklepias-main/lineFilter-test --file 10000groups-100lines-allfail.jsonl' \

hyperfine --warmup 1 \
    --min-runs=3 \
    --max-runs=3 \
    --parameter-list app testLineFilterApp-conduit,testLineFilterApp-fold \
    '{app} --dir hasklepias-main/lineFilter-test --file 10groups-100000lines.jsonl' \
    '{app} --dir hasklepias-main/lineFilter-test --file 100000groups-10lines.jsonl' \
    '{app} --dir hasklepias-main/lineFilter-test --file 10groups-100000lines-allfail.jsonl' \
    '{app} --dir hasklepias-main/lineFilter-test --file 100000groups-10lines-allfail.jsonl' 