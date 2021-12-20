#!/bin/sh

grep -e '^version:' "$1" | sed 's/version:[[:space:]]*//g'