# Tests for `cohortCollector` application

* `rw` = "row-wise"
* `cw` = "column-wise"

## Running tests locally

Install `cohort-collector`
(you may need to toggle the overwrite policy if previously installed)

```sh
cabal install cohort-collector 
```

```sh
cohort-collector -f cohort-collector/test/tests/manifestcw.txt -d cohort-collector
```

Or run using `cabal run`

```sh
cabal run cohort-collector:exe:cohort-collector -- -f cohort-collector/test/tests/manifestcw.txt -d cohort-collector
```

## Generating input test files

Test files were produces in the following way:

```sh
cabal build hasklepias-main

```sh
cat hasklepias-examples/exampleData/exampleData1.jsonl | cabal exec exampleCohortCwApp > cohort-collector/test/tests/testcw1.json
cat hasklepias-examples/exampleData/exampleData2.jsonl | cabal exec exampleCohortCwApp > cohort-collector/test/tests/testcw2.json
cat hasklepias-examples/exampleData/exampleData3.jsonl | cabal exec exampleCohortCwApp > cohort-collector/test/tests/testcw3.json
```

```sh
cat hasklepias-examples/exampleData/exampleData1.jsonl | cabal exec exampleCohortRwApp > cohort-collector/test/tests/testrw1.json
cat hasklepias-examples/exampleData/exampleData2.jsonl | cabal exec exampleCohortRwApp > cohort-collector/test/tests/testrw2.json
cat hasklepias-examples/exampleData/exampleData3.jsonl | cabal exec exampleCohortRwApp > cohort-collector/test/tests/testrw3.json
```

## Creating golden files

```sh
cohort-collector -f cohort-collector/test/tests/manifestcw.txt -d cohort-collector/test/tests/ > cohort-collector/test/tests/testcw.golden
```

```sh
cohort-collector -f cohort-collector/test/tests/manifestrw.txt -d cohort-collector/test/tests/ > cohort-collector/test/tests/testrw.golden
```

NOTE: you may need to remove a newline in the produced golden files.

## Uploading test files to S3

```sh
aws s3 sync  cohort-collector/test/tests/ s3://nsstatdev-main-usea1-projects-cohorts/sandbox/ --exclude "*" --include "*.json"
aws s3 sync  cohort-collector/test/tests/ s3://nsstatdev-main-usea1-projects-cohorts/sandbox/ --exclude "*" --include "s3manifest*"
```

## Running collector on S3 data examples

(whatever machine you run this on needs to have appropriate AWS credentials)

```sh
cohort-collector -b nsstatdev-main-usea1-projects-cohorts -m sandbox/s3manifestrw.txt
cohort-collector -b nsstatdev-main-usea1-projects-cohorts -m sandbox/s3manifestcw.txt
```
