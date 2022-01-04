#!/bin/bash
#
# Builds the asclepias documentation using cabal haddock.

# Create a location in which to copy the resulting files. This location will
# be saved as an artifact in the CI.
mkdir -p install/docs

# Create a location in which to place any temporary files.
TEMP=$(mktemp -d)

# Build documentation
cabal haddock all \
  --enable-documentation \
  --haddock-html-location="https://$(< ci/docs_path)$pkg/$version" \
  --haddock-hyperlink-source \
  --haddock-quickjump > "${TEMP}"/haddock-output.txt

# Get the paths to each documentation index.html in the haddock output
# TODO: surely there's a better way of getting the paths to the documentation
index_paths=$(grep doc/html "${TEMP}"/haddock-output.txt)

# For each path, copy the directory into in the artifact directory.
for path in $index_paths; do
	dir=$(dirname "$path")
	packageName=$(basename "$dir")
	packageVersion=$(./scripts/get-version-from-cabal.sh "$packageName"/"$packageName".cabal)

  mkdir -p "install/docs/${packageName}/${packageVersion}"
  mkdir -p "install/docs/${packageName}/latest"

  cp -R "$dir" "install/docs/${packageName}/${packageVersion}"
  cp -R "$dir" "install/docs/${packageName}/latest"
done
