#!/bin/sh
# Run hlint on .hs files.
# If run from the asclepias root directory, the app is configured by .hlint.yaml.
set -e

hlint . --ext=hs