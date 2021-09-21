# Tests for `cohortCollector` application

* `rw` = "row-wise"
* `cw` = "column-wise"

## Generating input test files

Test files were produces in the following way:

* The first file (e.g. `testrw1.json`) was generated from the `exampleApp` (with the shape compiled using `rowWise`):

```sh
cat exampleApp/exampleData.jsonl | cabal exec exampleApp
```

* The remaining files were edited by hand.

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
