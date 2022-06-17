#!/bin/bash
#
# Builds the asclepias documentation using cabal haddock.
set -e

# Create a location in which to copy the resulting files. If being run as part
# of the CI then require the presence of $HADDOCK_DIR, otherwise a fallback of
# `"install"` is provided as a convenience for local testing
[[ -n $GITLAB_CI ]] && [[ -z $HADDOCK_DIR ]] && exit 1
ARTIFACT_DIR="${HADDOCK_DIR:-install}"/docs
mkdir -p "$ARTIFACT_DIR"

# Create a location in which to place any temporary files.
TEMP=$(mktemp -d)

# Build documentation
# NOTEs:
# The --haddock-html-location value must be in single quotes;
# as we don't want substitution by the shell.
# See https://cabal.readthedocs.io/en/3.6/cabal-project.html#cfg-field-haddock-html-location
# shellcheck disable=SC2016
cabal haddock all \
  --enable-documentation \
  --haddock-html-location='https://docs.novisci.com/asclepias/$pkg/$version/$pkg' \
  --haddock-hyperlink-source \
  --haddock-quickjump > "${TEMP}"/haddock-output.txt

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
  mkdir -p "${ARTIFACT_DIR}/${packageName}/${packageVersion}"
  echo "${packageName}/${packageVersion}" >> "${ARTIFACT_DIR}"/manifest.txt
  mkdir -p "${ARTIFACT_DIR}/${packageName}/latest"
  echo "${packageName}/latest" >> "${ARTIFACT_DIR}"/manifest.txt

  cp -R "$dir" "${ARTIFACT_DIR}/${packageName}/${packageVersion}"
  cp -R "$dir" "${ARTIFACT_DIR}/${packageName}"/latest
done
