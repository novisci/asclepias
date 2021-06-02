# Changelog for hasklepias

## 0.8.3

* Modifies `FromJSON` instance for events to use `parseEvent` as well as create a moment from the provided `begin` in the case that `end` is missing.
* Updates `FromJSON` instance for events to parse the facts object. Currently the only implemented domain is demographics.
* Adds `FromJSON` instances for `Population`, so now data from multiple subjects can be marshaled into Hasklepias programs.

## 0.8.2

* Adds preliminary `Subject`, `Population`, `ObsUnit`, and `Cohort` types to the `Hasklepias` module, along with the `makeObsUnitFeatures` which takes a function that maps a `Subject` into a `ObsUnit`. Currently, this function only supports 1-1 mapping between subjects and observational units. There is also the `makeCohort` function which maps a `Population` to a `Cohort`. The types get preliminary `FromJSON` (for `Subject` and `Population`) and `ToJSON` (for `ObsUnit` and `Cohort`) instances as well. Example 1 demonstrates use of these two functions.

## 0.8.1

* Adds an initial pass at the `Domain` type; simply including the `Demographics` type for now.

## 0.8.0

* Reorganizes modules into a more "vertical" structure that reflects the decoupling of the various components that make up hasklepias:
  * `EventData`: types and functions related to the event data model
  * `FeatureCompose`: types and functions for composing new features
  * `FeatureEvents`: various utilities for composing features from events specifically
  * `Hasklepias`: at this point, just reexporting the above modules and other Haskell functions

## 0.7.3

* Refactors the `eval*` function for Features so that there is a single `eval` not two.

## 0.7.2

* Adds `Criteria` module which provides specialized functions and types for working with boolean `Feature`s which are meant to be used to determine whether a subject is included or excluded from cohorts.

## 0.7.1

* Fixes bug in `FeatureData` `Monad` instance, so you don't get infinite recursion.

## 0.7.0

* Modifies the way that `Feature`s are defined and evaluated. For one, the dependency between `Events` and `Feature`s is eliminated, thus decoupling the defining of Features from the input data type. There are now 4 functions for defining features:
`define0`, `define1`, `define2`, and `define2d`, and 3 functions for evaluating `eval0`, `eval1`, and `eval2` (and corresponding `evalSpec*`). These functions will hopefully be combined into a single interface at later time. See the function signatures for the types of functions passed to a `define*` function. Examples will be forthcoming as the API stabilizes.

## 0.6.1

* Adds `derving Eq` to `Feature` type.

## 0.6.0

* Adds `PolyKinds` extension to `Feature` module to enable poly-kind inputs to `FeatureDefinition`s. Adds a related `Defineable` typeclass with `define` and `eval` functions as a common interface for defining new definitions and evaluating them.
* Removes `defineEF` and `applyEF` function (and other similar functions). The functionality is now handled by the `Defineable` class.

## 0.5.1

* Adds `Show`, `Functor`, and `Generic` to Reexports.
* Updates `interval-algebra` to `0.8.2`.

## 0.5.0

* Changes what was the `Feature` type into `FeatureData`. The `Feature` type becomes a container for `FeatureData` with a name and attributes.
* Adds the `FeatureSpec` type which contains `FeatureDefinition`s plus a name and attributes. The name and attributes are mapped directly into the resulting `Feature` when a `FeatureSpec` is evaluated, while the `FeatureDefinition` is evaluated into `FeatureData`. The `evalEFFeature`, `evalFEFFeature`, and `evalFFFFeature` are provided for evaluating a `FeatureSpec` according the corresponding `FeatureDefinition`.
* Adds additional functions to reexports.
* Adds `witherable` dependency to use a more general `filter` function.

## 0.4.4

* Adds the `FFF` option to `FeatureDefinition` to define `(Feature f -> Feature e -> Feature d)` along with corresponding `defineFFF` and `applyFFF`.
* Adds `zipWith`, `id`, and `Integer` to re-exports.

## 0.4.3

* Exports `Feature` constructor.
* Adds `defineFEF2` function for creating a feature definition where the provided function returns a `Feature d` rather than just a `d`.
* Generalizes `allPairs` from type `[a] -> [a] -> [(a, a)]` to `[a] -> [b] -> [(a, b)]`.
* Reexports a few functions and types from `Data.Time.Calendar`. Also reexports `const` from `Data.Function`.

## 0.4.2

* Updates `interval-algebra` to 0.8.0.

## 0.4.1

* Modifies the example in `example/ExampleFeatures3` to use the pipe `|>` operator.
* Adds the `hasAllConcepts` function to the `HasConcepts` class.
* Adds a `Reexports` module with the goal to re-export everything one might need from other Haskell libraries to build a cohort.
* Removes a number of unneeded/unused functions from the `Functions` module.
* Adds the `Safe` language extension to modules where possible.

## 0.4.0

* Adds the `FeatureDefinition` to represent common patterns for building `Feature`s:

```haskell
data FeatureDefinition e a d =
    EF  (Events a -> Feature d)
  | FEF (Feature e -> Events a -> Feature d)
```

* Provides an initial set of functions designed to make defining `Feature`s easier, namely `defineEF` and `defineFEF`. These functions construct `FeatureDefinition`s of using `EF` and `FEF` constructors, respectively. The example features in `examples/ExampleFeatures1` demonstrate their use.
* Adds the `allPairs` function to form all pairs of elements of two lists.
* Adds the `splitByConcepts` to split a container of events into a pair such that first element contains
events have any of the first argument's concepts, and similarly for the second element.
* Demonstrates how `allPairs` and `splitByConcepts` might be used in the `examples/ExampleFeatures3` module.
* Adds a rudimentary `ToJSON` instance for `Feature`s so that data can be encoded and output from the software. This is pretty rough; e.g. encoding an `Interval Int` feature produces: `"{\"end\":10,\"begin\":0}"`.
* Removes the `Transformations` module and `transformToMeetingSequence` function. The same functionality is available by using the `formMeetingSequence` function from `interval-algebra`. See `examples/ExampleFeatures2` for the updated example.
* Adds the `toConceptEventOf` function which creates a `ConceptEvent` but takes the `intersection` of `Concepts` in the first argument and concepts in the context of the `Event` in the second argument to form the new `ConceptEvent`. This is a way to keep only those concepts you need in the event.

## 0.3.0

* Updates code as needed to work with interval-algebra v0.6.2. In particular, the `Event a` is now a synonym for `PairedInterval Context a`, hence any methods that work on the `PairedInterval` also work for the `Event` type.
* Adds the `ConceptEvent a` type which is a synonym for `PairedInterval Concept a`; i.e, this is an event without facts or a source.
* Adds the `toConceptEvent` function for dropping from an `Event a` to a `ConceptEvent a`, and `mkConceptEvent` function for directly making a `ConceptEvent` from concepts and an interval.
* Adds generators for lists of arbitrary events. The generator for `Concepts` is limited at this point; it simply takes a subsample of the first 10 letters of the alphabet. Currently, only generators for `Event Int` are provided by the `generateEventsInt`. For example, in the `repl` `generateEventsInt 2` produces two randomly generated events:

```haskell
*Hasklepias> generateEventsInt 2
[{(-33, -16), Context {getConcepts = fromList ["G","I"], getFacts = Nothing, getSource = Nothing}},{(12, 13), Context {getConcepts = fromList ["A","C","D","E","G","I"], getFacts = Nothing, getSource = Nothing}}]
```

* Adds the `transformToMeetingSequence` function which takes a set of concepts and a list of possibly non-disjoint `ConceptEvents`s and returns a list of `ConceptEvents`, where each consecutive event meets the next. Moreover, only those concepts selected (in the first argument) are kept in the output list of events. In the case that none of the events have the chosen concepts during an interval, an `ConceptEvent` with an empty set of concept is returned. A few examples might make this more clear.

```haskell
*Hasklepias> :set -XOverloadedStrings
*Hasklepias> x <- fmap (map toConceptEvent) (generateEventsInt 1)
*Hasklepias> x
[{(3, 4), fromList ["B","C"]}]
*Hasklepias> transformToMeetingSequence (map packConcept ["A"]) x
[{(3, 4), fromList []}]
*Hasklepias> transformToMeetingSequence (map packConcept ["B"]) x
[{(3, 4), fromList ["B"]}]
*Hasklepias> x <- fmap (map toConceptEvent) (generateEventsInt 10)
*Hasklepias> x
[{(-44, 7), fromList ["C","D","E","F","H","J"]},{(-30, -29), fromList ["A","B","F","G","H","I","J"]},{(-25, 5), fromList ["C","D","E","I"]},{(-20, -19), fromList ["A","C","E","G","I","J"]},{(-17, -16), fromList ["B","D","F","J"]},{(-6, -5), fromList ["E","F","H","J"]},{(2, 21), fromList ["A","F","J"]},{(18, 19), fromList ["D","F","G","H","I"]},{(19, 20), fromList ["B","C","D","E","F","H"]},{(30, 31), fromList ["B","C","D","H","J"]}]
*Hasklepias> transformToMeetingSequence (map packConcept ["B", "I"]) x
[{(-44, -30), fromList []},{(-30, -29), fromList ["B","I"]},{(-29, -25), fromList []},{(-25, -17), fromList ["I"]},{(-17, -16), fromList ["B","I"]},{(-16, 5), fromList ["I"]},{(5, 18), fromList []},{(18, 19), fromList ["I"]},{(19, 20), fromList ["B"]},{(20, 30), fromList []},{(30, 31), fromList ["B"]}]
```

* Adds an example of `transformToMeetingSequence` could be used to derive a feature that is the list of durations that a subject was both hospitalized and on antibiotics at the same time in the `examples/ExampleFeatures2` module.