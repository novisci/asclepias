
#!/bin/bash
# 

PKG=$1
COMPONENT=$2
VERSION=$(./scripts/get-version-from-cabal.sh ${PKG}/${PKG}.cabal)
RESULTPATH="$(./scripts/create-executable-build-path.sh $PKG $VERSION $PKG)"
INSTALLDIR=$3

cabal build ${PKG}:exe:${COMPONENT} \
   --constraint='text +integer-simple' \
   --constraint='cryptonite -integer-gmp' \
   --enable-executable-static \

strip $RESULTPATH 
cp $RESULTPATH $INSTALLDIR

#     - export COLLECTOR_BUNDLE=cohort-collector-$COLLECTORVERSION-linux-$(uname -m).tar.gz
# aws s3 cp $INSTALLDIR/$COLLECTOR_BUNDLE s3://download.novisci.com/hasklepias/$COLLECTOR_BUNDLE --acl public-read