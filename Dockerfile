FROM registry.novisci.com/nsstat/statocker/haskell:8.10.4
ARG version
RUN cabal update
RUN cabal install hasklepias-$version --lib