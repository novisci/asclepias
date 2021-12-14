#!/bin/sh
# Build a docker image in the CI.
#
# USAGE #
#
# The script takes 4 arguments:
# 1. the version with which to tag the image
# 2. name to give the resulting image
# 3. name of the statocker image used in the FROM directive of ci/Dockerfile
# 4. tag of the statocker image used in the FROM directive of ci/Dockerfile

set -e

echo $CI_REGISTRY_PASSWORD | \
  docker login \
  --username $CI_REGISTRY_USER \
  --password-stdin $CI_REGISTRY

NAME=$CI_REGISTRY_IMAGE/${2}

echo "Building the ${NAME} docker image"
echo "tagging image with ${1} and latest"
echo "using statocker image: ${3}:${4}"

docker build \
  --tag $NAME:$1 \
  --tag $NAME:latest \
  --build-arg STATOCKER=${3} \
  --build-arg GHC=${4} \
  --file ci/Dockerfile .

docker push $NAME:$1
docker push $NAME:latest
