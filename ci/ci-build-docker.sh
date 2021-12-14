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

NAME=$CI_REGISTRY_IMAGE/hasklepias-build

echo "Building the ${NAME} docker image"
echo "tagging image with ${1} and latest"
echo "using statocker image: ${3}:${4}"

docker build \
  --tag $NAME:$1 \
  --tag $NAME:latest \
  --build-arg STATOCKER=${3} \
  --build-arg GHC=${4} \
  --file $2 .

docker push $NAME:$1
docker push $NAME:latest
