#!/bin/bash
#
# Run checks on the consistency of the documentation
set -e

TEMP=$(mktemp -d)

## Compare the help text for the cohort collector application
COLLECTORAPP=$(<install/cohort-collector.name)
# Don't want to diff 'Usage: cohort-collector-{version}-{blah}' so copy binary
# into cohort-collector
cp install/"${COLLECTORAPP}" install/cohort-collector
./install/cohort-collector --help >"${TEMP}"help.txt
diff -Bb "${TEMP}"help.txt cohort-collector/help.txt

rm -r "${TEMP}"
