#!/bin/bash
#
# Builds the asclepias documentation using cabal haddock.
set -e

# Create a location in which to copy the resulting files.
# If being run as part
# of the CI then require the presence of $HADDOCK_DIR,
# otherwise a fallback of
# `"install"` is provided as a convenience for local testing
[[ -n $GITLAB_CI ]] && [[ -z $HADDOCK_DIR ]] && exit 1
ARTIFACT_DIR="${HADDOCK_DIR:-install}"/docs
mkdir -p "$ARTIFACT_DIR"

# Create a location in which to place any temporary files.
TEMP=$(mktemp -d)

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
  all > "${TEMP}"/haddock-output.txt

# Get the paths to each documentation index.html in the haddock output
# TODO: surely there's a better way of getting the paths to the documentation
index_paths=$(grep doc/html "${TEMP}"/haddock-output.txt)

# For each path, copy the directory into in the artifact directory and add the 
# directory to a manifest which can be used in the downstream upload job.
for path in $index_paths; do
	dir=$(dirname "$path")
	packageName=$(basename "$dir")
	packageVersion=$(./scripts/get-version-from-cabal.sh "$packageName"/"$packageName".cabal)

  touch "${ARTIFACT_DIR}"/manifest.txt
  echo "${packageName}/${packageVersion}" >> "${ARTIFACT_DIR}"/manifest.txt

  cp -r "$dir" "${ARTIFACT_DIR}"
done

# Build a single "directory" pages for each package
haddock \
  -o "${ARTIFACT_DIR}" \
  --quickjump --gen-index --gen-contents \
  --read-interface=event-data-theory,"${ARTIFACT_DIR}"/event-data-theory/event-data-theory.haddock \
  --read-interface=hasklepias-core,"${ARTIFACT_DIR}"/hasklepias-core/hasklepias-core.haddock \
  --read-interface=hasklepias-main,"${ARTIFACT_DIR}"/hasklepias-main/hasklepias-main.haddock \
  # TODO Commmented out as part of resolving #357 in !295
  #--read-interface=hasklepias-examples,"${ARTIFACT_DIR}"/hasklepias-examples/hasklepias-examples.haddock \
  #--read-interface=cohort-collector,"${ARTIFACT_DIR}"/cohort-collector/cohort-collector.haddock
