#!/bin/sh
#
# This script does the following:
# 1. builds a statically linked executable.
# 2. strips the executable.
# 3. copies, tars, and gzips the result to an installation directory
# 4. copies the result to S3
#
# USAGE
#
# Takes 3 arguments:
# 1. name of the project package
# 2. name of the executable component within the package to build
# 3. directory in which to place the result

# Ensure we have exactly three positional arguments
if [ $# -ne 3 ]; then
    1>&2 echo 'error: expects 3 positional arguments'
    exit 1
fi

PKG=$1
COMPONENT=$2
INSTALLDIR=$3

VERSION=$(./scripts/get-version-from-cabal.sh "${PKG}"/"${PKG}".cabal)
ARCH=$(uname -m)
SYS=$(uname -s | tr '[:upper:]' '[:lower:]')
NAME=${COMPONENT}-${VERSION}-${ARCH}-${SYS}
BUNDLE=${NAME}.tar.gz

mkdir -p "$INSTALLDIR"

# The commented-out constraints were at one point necessary, but now lead to an
# error for recent builds (as of 2022-02-01 - DP)
#
# BS: As of 2022-03-02, the constraints are necessary (or seem to be) again
# see 
# https://gitlab.novisci.com/nsStat/asclepias/-/jobs/72502
# for a failed build without constraints, and
# https://gitlab.novisci.com/nsStat/asclepias/-/jobs/72650
# for a successful build with constraints
cabal build "${PKG}":exe:"${COMPONENT}" \
   -j \
   -O2 \
   --enable-executable-static \
   --constraint='text +integer-simple' \
   --constraint='cryptonite -integer-gmp' \
   || exit 1


# Get the path to the executable
EXE="$(./scripts/create-executable-build-path.sh "$PKG" "$VERSION" "$PKG")"

# Remove symbols from the executable
strip "$EXE" || exit 1
apt install -y file  # TODO: remove this once `file` makes it into the Haskell docker container
if ! file "$EXE" | grep 'statically linked'; then
    1>&2 echo 'error: the following file is not statically linked:'
    1>&2 echo "$EXE"
    exit 1
fi

cp "$EXE" "${INSTALLDIR}"/"${NAME}" || exit 1

tar -czvf "$BUNDLE" "${INSTALLDIR}"/"${NAME}" || exit 1

mv "$BUNDLE" "$INSTALLDIR"/"${BUNDLE}" || exit 1

echo "$VERSION" >"${INSTALLDIR}"/"${COMPONENT}".version
echo "$NAME" >"${INSTALLDIR}"/"${COMPONENT}".name
echo "$BUNDLE" >"${INSTALLDIR}"/"${COMPONENT}".bundle
