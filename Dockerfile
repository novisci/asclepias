FROM registry.novisci.com/nsstat/statocker/haskell:8.10.4

COPY cabal.project .
COPY core/hasklepias.cabal .
COPY collector/collector.cabal .
COPY edm/edm.cabal .
COPY stype/stype.cabal .

RUN cabal update
RUN cabal build all --only-dependencies