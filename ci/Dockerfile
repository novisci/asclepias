ARG STATOCKER=haskell
ARG GHC=8.10.7

FROM registry.novisci.com/nsstat/statocker/$STATOCKER:$GHC

ARG STATOCKER 
ARG GHC

WORKDIR /project

# TODO: surely there's a clean way to copy in necessary files
COPY cabal.project .
COPY hasklepias-core/hasklepias-core.cabal hasklepias-core/hasklepias-core.cabal
COPY hasklepias-templates/hasklepias-templates.cabal hasklepias-templates/hasklepias-templates.cabal
COPY hasklepias-appBuilder/hasklepias-appBuilder.cabal hasklepias-appBuilder/hasklepias-appBuilder.cabal
COPY hasklepias-main/hasklepias-main.cabal hasklepias-main/hasklepias-main.cabal
COPY cohort-collector/cohort-collector.cabal cohort-collector/cohort-collector.cabal
COPY edm/edm.cabal edm/edm.cabal
COPY stype/stype.cabal stype/stype.cabal

RUN cabal update

COPY ci/ci-cabal-build-only-deps.sh ci/ci-cabal-build-only-deps.sh
RUN ./ci/ci-cabal-build-only-deps.sh $STATOCKER