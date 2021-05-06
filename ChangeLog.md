# Changelog for hasklepias

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
