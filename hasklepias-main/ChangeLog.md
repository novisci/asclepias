# Changelog for hasklepias-main

## 0.27.0

* Adds a benchmarking application and some benchmarking routines to `Tests.Hasklepias` for `Hasklepias.AppBuilder.LineFilterApp`.
* Replaces with `Hasklepias.MakeFilterApp` with `Hasklepias.AppBuilder.LineFilterApp`, refactoring the filter app in the process and fixing unexpected behavior when piping filter app output to aws S3.
* Adds `pretty-simple` dependency and reexports its `pPrint` function.
From the docs: "`pretty-simple` is a pretty printer
for Haskell data types that have a Show instance."
* Adds explicit dependency bounds in `asclepias` project `package.cabal` files for `interval-algebra` and `time` instead of providing the bounds solely in `cabal.project`, to ensure downstream discoverability of those bounds.

## 0.26.0

* Update to `interval-algebra-2.0`.
* Refactor `cabal.project` and individual package dependency bounds.
The project now pins a Hackage index state and a compiler version.

## 0.25.0

* Combines reexports modules into a single module.
* Removes `setFromList`, `mapToList`, `mapFromList` functions.
Use witch's `from` or `into` functions with type application instead.

## 0.24.0

* This version includes many updates to associated packages including:
* The `event-data-model` package has been completely removed from the repository
in favor of the `event-data-theory` package.
The `event-data-theory` modules introduce a generic type for event.
In past versions, the event type was `Event a`,
where the context (i.e. "data") part of the event was fixed
to the event data model v1.*.
Now, the event type take two additional parameters: `Event c m a`,
The type parameters `m` and `c` allow a user to specify
the types for the 'Context's `m`odel and `c`oncepts.
In short, the `Event` type is much more flexible.
* Documentation for `asclepias` has been added to the
[noviverse](https://docs.novisci.com/noviverse/index.html)
* Many other updates which are tracked
[on GitLab](https://gitlab.novisci.com/nsStat/asclepias/-/milestones/1#tab-issues)

## 0.22.5

* Bumping version for updates to the CI and internal packages.
* Move examples and tests into `hasklepias-examples`.

## 0.22.4

* Adds `Hasklepias.ExampleFilterApp` module as an example application
that filters subjects have at least one event
that satisfies a given predicate function.
* Adds a test suite for the `exampleFilterApp`.

## 0.22.3

* Updates examples to run with updates to `hasklepias-core`.

## 0.22.2

* Adds tests for the example application.

## 0.22.1

* Updates examples to work with `hasklepias-core` `0.22.0`.
* In preparation for a proper testing suite for the example app, moves the app part of the `exampleApp` code into to `Hasklepias.ExampleApp`.

## 0.22.0

* Splits `hasklepias-main` into a separate project from `hasklepias-core`.
