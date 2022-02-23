#!/bin/sh

# Ensure that $1 is nonempty and has exactly 1 line. See
# https://stackoverflow.com/a/54746843/5518304
n_lines=$(printf "%s" "$1" | grep -c "^")
if [ "$n_lines" -eq 0 ]; then
    1>&2 echo "error: empty variable"
    exit 1
elif [ "$n_lines" -ge 2 ]; then
    1>&2 echo 'error: found more than one line'
    1>&2 echo "$1"
    exit 1
fi
