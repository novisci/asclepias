#!/bin/bash

ARTIFACT_DIR=install/docs

aws s3 sync \
		"${ARTIFACT_DIR}" \
		s3://docs.novisci.com/asclepias/api/ \
		--delete --acl public-read \

aws cloudfront create-invalidation \
	--distribution-id E171A73GHZQS0B \
	--paths /asclepias/api-docs
