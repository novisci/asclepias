#!/bin/bash
#
# Builds the asclepias documentation using cabal haddock.
set -e

# default is provided as a convenience for local testing
if [[ -z $ARTIFACT_DIR ]]; then
	if [[ -n $GITLAB_CI ]]; then
		echo "Error: ARTIFACT_DIR must be set when GITLAB_CI=$GITLAB_CI present"
		exit 1
	else
		ARTIFACT_DIR="install/doc"
	fi
fi

echo "Creating $ARTIFACT_DIR"
mkdir -p "$ARTIFACT_DIR"

# Create a location in which to place any temporary files.
TEMP=$(mktemp -d)

# #
# Build documentation
# NOTEs:
# This page was quite helpful in getting the cabal haddock
# command to build a single site
# https://haskell-haddock.readthedocs.io/en/latest/multi-components.html
# shellcheck disable=SC2016
cabal haddock \
	--haddock-hyperlink-source \
	--haddock-quickjump \
	--haddock-html \
	--haddock-html-location='https://docs.novisci.com/asclepias/api/$pkg/$version/$pkg' \
	--haddock-option=--lib=../resources/haddock-api-resources/ \
	--haddock-option=--use-index=../doc-index.html \
	--haddock-option=--use-contents=../index.html \
	--haddock-option='--base-url=..' \
	all >"${TEMP}"/haddock-output.txt

#
echo "Completed cabal haddock"

# Get the paths to each documentation index.html in the haddock output
echo "Setting index_paths"
index_paths=$(grep doc/html "${TEMP}"/haddock-output.txt)

# For each path, copy the directory into in the artifact directory and add the
# directory to a manifest which can be used in the downstream upload job.
echo "Copying dirs in index_paths"
for path in $index_paths; do
	echo "Processing $path"
	dir=$(dirname "$path")
	packageName=$(basename "$dir")
	packageVersion=$(./scripts/get-version-from-cabal.sh "$packageName"/"$packageName".cabal)

	touch "${ARTIFACT_DIR}"/manifest.txt
	echo "${packageName}/${packageVersion}" >>"${ARTIFACT_DIR}"/manifest.txt

	cp -r "$dir" "${ARTIFACT_DIR}"
done

# Build a single "directory" pages for each package
echo "Running haddock"
haddock \
	-o "${ARTIFACT_DIR}" \
	--quickjump --gen-index --gen-contents \
	--read-interface=event-data-theory,"${ARTIFACT_DIR}"/event-data-theory/event-data-theory.haddock \
	--read-interface=hasklepias-core,"${ARTIFACT_DIR}"/hasklepias-core/hasklepias-core.haddock \
	--read-interface=hasklepias-main,"${ARTIFACT_DIR}"/hasklepias-main/hasklepias-main.haddock

# TODO: Commmented out as part of resolving #357 in !295
#--read-interface=hasklepias-examples,"${ARTIFACT_DIR}"/hasklepias-examples/hasklepias-examples.haddock \
#--read-interface=cohort-collector,"${ARTIFACT_DIR}"/cohort-collector/cohort-collector.haddock

# TODO: not the way. for debugging, this upload to s3 script we need the ARTIFACT_DIR
