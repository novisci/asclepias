#!/bin/sh
# 
# Run checks on the consistency of the documentation
set -e

TEMP=$( mktemp -d )

## Compare the help text for the cohort collector application
cabal exec -- cohort-collector --help > ${TEMP}help.txt
diff ${TEMP}help.txt cohort-collector/help.txt

rm -r ${TEMP}