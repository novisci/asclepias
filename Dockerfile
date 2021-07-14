FROM registry.novisci.com/nsstat/statocker/haskell:8.10.4
# ARG version
RUN cabal update
COPY hasklepias.cabal .
RUN cabal build --only-dependencies