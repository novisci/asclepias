# Changelog for hasklepias-main

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
