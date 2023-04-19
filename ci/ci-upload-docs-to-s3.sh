#!/bin/bash

#ARTIFACT_DIR=install/docs

set -e

echo "ARTIFACT_DIR=$ARTIFACT_DIR"

aws s3 sync \
	"${ARTIFACT_DIR}" \
	s3://docs.novisci.com/asclepias/api/ \
	--delete --acl public-read

echo "exit code from aws s3 sync: $?"

aws cloudfront create-invalidation \
	--distribution-id E171A73GHZQS0B \
	--paths /asclepias/api-docs
