#!/bin/sh
# Build a docker image in the CI.
#
# USAGE #
#
# The script takes 4 arguments:
# 1. the version with which to tag the image
# 2. name to give the resulting image
# 3. name of the nsBuild image used in the FROM directive of ci/Dockerfile
# 4. tag of the nsBuild image used in the FROM directive of ci/Dockerfile

set -e

echo "$CI_REGISTRY_PASSWORD" |
  docker login \
    --username "$CI_REGISTRY_USER" \
    --password-stdin "$CI_REGISTRY"

# The $CI_REGISTRY_IMAGE environmental variable takes the prefix of the
# container registry associated with the GitLab project, e.g.
# `registry.gitlab.com/targetrwe/epistats/nsstat/asclepias`
NAME=$CI_REGISTRY_IMAGE/${2}

echo "Building the ${NAME} docker image"
echo "tagging image with ${1} and latest"
echo "using nsBuild image: ${3}:${4}"

docker build \
  --tag "$NAME":"$1" \
  --tag "$NAME":latest \
  --build-arg BASE_IMAGE="${3}" \
  --build-arg GHC="${4}" \
  --file ci/dockerHasklepias .

docker push "$NAME":"$1"
docker push "$NAME":latest
