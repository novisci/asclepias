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

PKG=$1
COMPONENT=$2
VERSION=$(./scripts/get-version-from-cabal.sh "${PKG}"/"${PKG}".cabal)
EXE="$(./scripts/create-executable-build-path.sh "$PKG" "$VERSION" "$PKG")"
INSTALLDIR=$3
ARCH=$(uname -m)
SYS=$(uname -s | tr '[:upper:]' '[:lower:]')
NAME=${COMPONENT}-${VERSION}-${ARCH}-${SYS}
BUNDLE=${NAME}.tar.gz

mkdir -p "$INSTALLDIR"

# The commented-out constraints were at one point necessary, but now lead to an
# error for recent builds (as of 2022-02-01 - DP)
cabal build "${PKG}":exe:"${COMPONENT}" \
   -j \
   -O2 \
   --enable-executable-static
   # --constraint='text +integer-simple' \
   # --constraint='cryptonite -integer-gmp' \

strip "$EXE"
if ! file "$EXE" | grep 'statically linked'; then
    1>&2 echo 'error: the following file is not statically linked:'
    1>&2 echo "$EXE"
    exit 1
fi

cp "$EXE" "${INSTALLDIR}"/"${NAME}"

tar -czvf "$BUNDLE" "${INSTALLDIR}"/"${NAME}"

mv "$BUNDLE" "$INSTALLDIR"/"${BUNDLE}"

echo "$VERSION" >"${INSTALLDIR}"/"${COMPONENT}".version
echo "$NAME" >"${INSTALLDIR}"/"${COMPONENT}".name
echo "$BUNDLE" >"${INSTALLDIR}"/"${COMPONENT}".bundle
