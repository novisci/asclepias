# Changelog for event-data-theory

## 0.30.2

* Fixes an incorrect implementation of `Ord` for `Event`.

## 0.30.1

* Changes the `valid` field of a `FactLine` to `Maybe Bool` from `Maybe Text`.

## 0.29.0

* updates to `FactsLine` type to conform to changes in `event-data-model`
* adds doctests
* gitlab migration updates
* switch from brittany to stylish-haskell

## 0.28.0

* updates `interval-algebra` to 2.1.1.

## 0.27.0
* Fix compiler warning about `Arbitrary Event` instance after update to interval-algebra 2.0.

## 0.26.0
* Update to interval-algebra 2.0

## 0.2.0

* Adds to `ToJSON` instance for `EventLine`.
* Changes the first 4 elements of an `EventLine` to simply the opaque `Value`.
Data in these slots is expected to be created by upstream consumers.
* Removes the `From` instance for `EventLine` to `Event` in favor of
a `TryFrom` instance,
where `tryFrom` takes a (`EventLine`, `ParseEventLineOption`) pair.
The `ParseEventLineOption` is used to modify the `TimeLine` object of the `EventLine`
before attempting to `parseInterval`.
Furthermore, the step of parsing an `Interval` in the `EventLine` `FromJSON` instance was removed.
* Updates all the decode functions to handle the `ParseEventLineOption`.
* Adds the `modifyEventLineWithContext` function for modifying an `EventLine` via its corresponding `Event`'s  `Context` representation.
* Adds the `eventLineRoundTripTests` function
for testing that `EventLine` JSON can roundtrip isomorphically to/from JSON.
* Adds the `eventLineModifyTests` function
for testing that `EventLine` JSON is unmodified by `modifyEventLine id`.
* Adds `context` smart constructor and removed export of `MkContext`.
* Adds `trimapEvent`, `bimapContext`, and `mapConcepts` functions for functor-like
application to the `Event`, `Context`, and `Concepts` type respectively.
* Adds the `dropSource` utility for setting the `getSource` in a `Context` to `Nothing`.
* Adds the `addConcepts` utility for adding a list of `c` to a `Concepts c`.
* Adds the `EventFunction` class for lifting a function on a component of an `Event` to a function on an `Event`.
