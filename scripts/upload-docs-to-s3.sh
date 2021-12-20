#!/bin/sh

haddockoutput=$1
paths=$(grep doc/html "$haddockoutput")

for path in $paths; do
	dir=$(dirname "$path")
	packageName=$(basename "$dir")
	packageVersion=$(./scripts/get-version-from-cabal.sh "$packageName"/"$packageName".cabal)

	echo "$packageName"
	echo "$packageVersion"

	aws s3 sync "$dir" s3://docs.novisci.com/asclepias/"$packageName"/"$packageVersion"/ --delete --acl public-read
	aws s3 sync "$dir" s3://docs.novisci.com/asclepias/"$packageName"/latest/ --delete --acl public-read
	aws cloudfront create-invalidation --distribution-id E171A73GHZQS0B --paths /asclepias/"$packageName"/latest
done
