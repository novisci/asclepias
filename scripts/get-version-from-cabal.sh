#!/bin/sh

version=$(grep -e '^version:' "$1" | sed 's/version:[[:space:]]*//g')
scripts/assert-exactly-one-line.sh "$version"
echo "$version"
