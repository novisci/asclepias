FROM registry.novisci.com/nsstat/statocker/haskell:8.10.4
# ARG version
COPY hasklepias.cabal .
RUN cabal update
RUN cabal build --only-dependencies