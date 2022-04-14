#!/bin/bash

ARTIFACT_DIR=install/docs
doc_dirs=$(< ${ARTIFACT_DIR}/manifest.txt )

for dir in $doc_dirs; do
	aws s3 sync "${ARTIFACT_DIR}/$dir" s3://docs.novisci.com/asclepias/"$dir"/ --delete --acl public-read
	aws cloudfront create-invalidation --distribution-id E171A73GHZQS0B --paths /asclepias/"$dir"
done
