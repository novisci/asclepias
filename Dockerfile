FROM registry.novisci.com/nsstat/statocker/haskell:8.10.4

COPY cabal.project .
COPY core/hasklepias.cabal core/.
COPY collector/collector.cabal collector/.
COPY edm/edm.cabal edm/.
COPY stype/stype.cabal stype/.

RUN cabal update
RUN cabal build all --only-dependencies