# Changelog for event-data-theory

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