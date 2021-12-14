#!/bin/sh
# Build a docker image in the CI.
#
# USAGE #
#
# The script takes 2 arguments:
# 1. the version with which to tag the image
# 2. name of the dockerfile
# 3. name of the statocker image
# 4. tag of the statocker image

set -e

echo $CI_REGISTRY_PASSWORD | \
  docker login \
  --username $CI_REGISTRY_USER \
  --password-stdin $CI_REGISTRY

NAME=$CI_REGISTRY_IMAGE/${5:-hasklepias}-build

docker build \
  --tag $NAME:$1 \
  --tag $NAME:latest \
  --build-arg statocker_image=${3:-haskell} \
  --build-arg ghc_version=${4:-8.10.7} \
  --file $2 .

docker push $NAME:$1
docker push $NAME:latest
