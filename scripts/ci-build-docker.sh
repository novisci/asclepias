#!/bin/sh
# Build a docker image in the CI
set -e

echo $CI_REGISTRY_PASSWORD | \
  docker login \
  --username $CI_REGISTRY_USER \
  --password-stdin $CI_REGISTRY

NAME=$CI_REGISTRY_IMAGE/${3:-hasklepias}-build

docker build \
  --tag $NAME:$1 \
  --tag $NAME:latest \
  --file $2 .

docker push $NAME:$1
docker push $NAME:latest
