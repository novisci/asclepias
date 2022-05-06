#!/bin/sh
# Lint
# If run from the asclepias root directory, the hlint app is configured by 
# .hlint.yaml.
set -e

hlint . --ext=hs

shellcheck scripts/*.sh
shellcheck ci/*.sh