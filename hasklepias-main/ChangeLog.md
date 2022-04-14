# Changelog for hasklepias-main

## 0.22.5

* Bumping version for updates to the CI and internal packages.
* Move examples and tests into `hasklepias-examples`.

## 0.22.4

* Adds `Hasklepias.ExampleFilterApp` module as an example application that filters subjects have at least one event that satisfies a given predicate function. 
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
