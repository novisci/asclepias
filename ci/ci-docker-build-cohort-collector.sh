#!/bin/sh
# Build a docker image for the cohort collector in the CI.
#
# USAGE #
#
# The script takes 2 arguments:
# 1. the version with which to tag the image
# 2. the path to the executable

set -e

echo $CI_REGISTRY_PASSWORD | \
  docker login \
  --username $CI_REGISTRY_USER \
  --password-stdin $CI_REGISTRY

NAME=$CI_REGISTRY_IMAGE/cohort-collector

echo "Building the ${NAME} docker image"

docker build \
  --tag $NAME:$1 \
  --tag $NAME:latest \
  --build-arg file=$2 \
  --file ci/dockerCohortCollector .

docker push $NAME:$1
docker push $NAME:latest
