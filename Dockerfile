FROM registry.novisci.com/nsstat/nsbuild/haskell:8.10.7

COPY cabal.project .
COPY hasklepias-core/hasklepias-core.cabal hasklepias-core/hasklepias-core.cabal
COPY hasklepias-templates/hasklepias-templates.cabal hasklepias-templates/hasklepias-templates.cabal
COPY hasklepias-main/hasklepias-main.cabal hasklepias-main/hasklepias-main.cabal
COPY cohort-collector/cohort-collector.cabal cohort-collector/cohort-collector.cabal
COPY event-data-theory/event-data-theory.cabal event-data-theory/event-data-theory.cabal
COPY event-data-model/event-data-model.cabal event-data-model/event-data-model.cabal
COPY stype/stype.cabal stype/stype.cabal

RUN cabal update
RUN cabal build all --only-dependencies

RUN apt install -y file
