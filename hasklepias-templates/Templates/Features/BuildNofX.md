---
title: buildNofX
tags: []
---

## Description

Do N events relating to the assessment interval in some way the satisfy the given predicate?

```haskell
module Templates.Features.BuildNofX
  ( buildNofX
  , buildNofXBool
  , buildNofXBinary
  , buildNofXBinaryConcurBaseline
  , buildNofConceptsBinaryConcurBaseline
  , buildNofXTests
  ) where

import           Templates.FeatureReqs
import           Templates.Features.BuildNofXBase
```

## Usage

TODO

## Definition

```haskell
buildNofX
  :: (Intervallic i a, Witherable container)
  => (Bool -> outputType) -- ^ casting function
  -> Natural -- ^ minimum number of cases
  -> (Index i a -> AssessmentInterval a) -- ^ function to transform a 'Cohort.Index' to an 'Cohort.AssessmentInterval'
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a) -- ^ interval predicate
  -> Predicate (Event a) -- ^ a predicate on events
  -> Definition
       (  Feature indexName (Index i a)
       -> Feature eventsName (container (Event a))
       -> Feature varName outputType
       )
buildNofX f n = buildNofXBase id (\x -> length x >= naturalToInt n) (const f)
```

### Specialized Definitions

`buildNofX` specialized to return `Binary`.

```haskell
buildNofXBinary
  :: (Intervallic i a, Witherable container)
  => Natural
  -> (Index i a -> AssessmentInterval a)
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a)
  -> Predicate (Event a)
  -> Definition
       (  Feature indexName (Index i a)
       -> Feature eventsName (container (Event a))
       -> Feature varName Binary
       )
buildNofXBinary = buildNofX fromBool
```

`buildNofX` specialized to return a boolean.

```haskell
buildNofXBool
  :: (Intervallic i a, Witherable container)
  => Natural -- ^ minimum number of cases 
  -> (Index i a -> AssessmentInterval a) -- ^ function to transform a 'Cohort.Index' to an 'Cohort.AssessmentInterval'
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a) -- ^ interval predicate
  -> Predicate (Event a) -- ^ a predicate on events
  -> Definition
       (  Feature indexName (Index i a)
       -> Feature eventsName (container (Event a))
       -> Feature varName Bool
       )
buildNofXBool = buildNofX id
```

`buildNofXBinary` specialized to filter to events that concur with an assessment interval created by `makeBaselineFromIndex` of a specified duration and a provided predicate.

```haskell
buildNofXBinaryConcurBaseline
  :: (Intervallic i0 a, Witherable t, IntervalSizeable a b, Baseline i0 a)
  => Natural -- ^ minimum number of events.
  -> b -- ^ duration of baseline (passed to 'Cohort.makeBaselineFromIndex')
  -> Predicate (Event a)
  -> Definition
       (  Feature indexName (Index i0 a)
       -> Feature eventsName (t (Event a))
       -> Feature varName Binary
       )
buildNofXBinaryConcurBaseline n baselineDur =
  buildNofXBinary n (makeBaselineFromIndex baselineDur) concur
```

`buildNofXBinary` specialized to filter to events that concur with an assessment interval created by `makeBaselineFromIndex` of a specified duration and that have a given set of concepts.

```haskell
buildNofConceptsBinaryConcurBaseline
  :: (Intervallic i0 a, Witherable t, IntervalSizeable a b, Baseline i0 a)
  => Natural -- ^ minimum number of events. 
  -> b  -- ^ duration of baseline (passed to 'Cohort.makeBaselineFromIndex')
  -> [Text] -- ^ list of 'EventData.Concepts' passed to 'EventData.containsConcepts'
  -> Definition
       (  Feature indexName (Index i0 a)
       -> Feature eventsName (t (Event a))
       -> Feature varName Binary 
       )
buildNofConceptsBinaryConcurBaseline n baselineDur cpts = buildNofXBinary
  n
  (makeBaselineFromIndex baselineDur)
  concur
  (containsConcepts cpts)
```


## Examples

```haskell

type NofXArgs
  = ( Natural
    , Index Interval Int -> AssessmentInterval Int
    , ComparativePredicateOf2 (AssessmentInterval Int) (Event Int)
    , Predicate (Event Int)
    )

type NofXTestCase
  = TestCase
      (F "index" (Index Interval Int), F "events" [Event Int])
      Bool
      NofXArgs

buildNofXTestCases :: [NofXTestCase]
buildNofXTestCases =
  [ f "False if no events"
      (1, makeBaselineFromIndex 10, concur, isEnrollmentEvent)
      (0, 1)
      []
      False
  , f
    "False if 1 event after index but looking for single event concurring with baseline"
    (1, makeBaselineFromIndex 10, concur, isEnrollmentEvent)
    (0, 1)
    [g (2, 7)]
    False
  , f
    "True if 1 event before index and looking for single event concurring with baseline"
    (1, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
    (0, 1)
    [h ["A", "B"] (-5, -4)]
    True
  , f
    "True if 2 events before index and looking for at least 2 events concurring with baseline"
    (2, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
    (0, 1)
    [h ["A", "B"] (-5, -4), h ["A", "C"] (-3, -2)]
    True
  , f
    "True if 3 events before index and looking for at least 2 events concurring with baseline"
    (2, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
    (0, 1)
    [h ["A", "B"] (-7, -6), h ["A", "B"] (-5, -4), h ["A", "C"] (-3, -2)]
    True
  , f
    "True if 2 events of same interval before index and looking for at least 2 events concurring with baseline"
    (2, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
    (0, 1)
    [h ["A"] (-5, -4), h ["A", "B"] (-5, -4)]
    True
  , f
    "False if 1 event before index and looking for at least 2 events concurring with baseline"
    (2, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
    (0, 1)
    [h ["A", "C"] (-3, -2)]
    False
  ] where
  f = makeTestInputs
  g = makeEnrollmentEvent
  h = makeEventWithConcepts

buildNofXTests :: TestTree
buildNofXTests = testGroup
  "Tests of NofX template"
  (fmap
    (\x -> testCase
      (getTestName x)
      (makeAssertion x (uncurryN $ eval (uncurryN (buildNofX id) (getBuilderArgs x))))
    )
    buildNofXTestCases
  )
```
