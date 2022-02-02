#!/bin/sh

version=$(grep -e '^version:' "$1" | sed 's/version:[[:space:]]*//g')
if ! [ $? ]; then
    1>&2 echo "error: unable to extract the version number from $1"
    exit 1
fi
echo "$version"
