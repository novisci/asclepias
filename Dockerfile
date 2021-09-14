FROM registry.novisci.com/nsstat/statocker/haskell:8.10.4

COPY cabal.project .
COPY core/hasklepias.cabal core/hasklepias.cabal
COPY collector/collector.cabal collector/collector.cabal
COPY edm/edm.cabal edm/edm.cabal
COPY stype/stype.cabal stype/stype.cabal

RUN cabal update
RUN cabal build all --only-dependencies