#!/bin/bash
#
# Builds the asclepias documentation using cabal haddock.

# Create a location in which to copy the resulting files. This location will
# be saved as an artifact in the CI.
ARTIFACT_DIR=install/docs
mkdir -p $ARTIFACT_DIR 

# Create a location in which to place any temporary files.
TEMP=$(mktemp -d)

# Just dummy variables to avoid ShellCheck hint
pkg= 
version=

# Build documentation
cabal haddock all \
  --enable-documentation \
  --haddock-html-location="https://$(< ci/docs_path)$pkg/$version" \
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

  touch ${ARTIFACT_DIR}/manifest.txt
  mkdir -p "${ARTIFACT_DIR}/${packageName}/${packageVersion}"
  echo "${packageName}/${packageVersion}" >> ${ARTIFACT_DIR}/manifest.txt
  mkdir -p "${ARTIFACT_DIR}/${packageName}/latest"
  echo "${packageName}/latest" >> ${ARTIFACT_DIR}/manifest.txt

  cp -R "$dir" "${ARTIFACT_DIR}/${packageName}/${packageVersion}"
  cp -R "$dir" "${ARTIFACT_DIR}/${packageName}/latest"
done
