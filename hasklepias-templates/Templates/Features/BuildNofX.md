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

import           Data.Text                         ( Text )
import           GHC.Natural                       ( Natural, naturalToInt )
import           Templates.FeatureReqs
import           Templates.Features.BuildNofXBase
import           Test.Tasty                        ( TestTree )
import           Witherable                        ( Witherable )
```

## Usage

TODO

## Definition

```haskell
buildNofX
  :: (Intervallic i a, Witherable container)
  => (Bool -> outputType) -- ^ casting function
  -> Natural -- ^ minimum number of cases
  -> (i a -> AssessmentInterval a) -- ^ function to transform a 'Cohort.Index' to an 'Cohort.AssessmentInterval'
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event ClaimsSchema c a) -- ^ interval predicate
  -> Predicate (Event ClaimsSchema c a) -- ^ a predicate on events
  -> Definition
       (  Feature indexName (i a)
       -> Feature eventsName (container (Event ClaimsSchema c a))
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
  -> (i a -> AssessmentInterval a)
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event ClaimsSchema c a)
  -> Predicate (Event ClaimsSchema c a)
  -> Definition
       (  Feature indexName (i a)
       -> Feature eventsName (container (Event ClaimsSchema c a))
       -> Feature varName Binary
       )
buildNofXBinary = buildNofX fromBool
```

`buildNofX` specialized to return a boolean.

```haskell
buildNofXBool
  :: (Intervallic i a, Witherable container)
  => Natural -- ^ minimum number of cases 
  -> (i a -> AssessmentInterval a) -- ^ function to transform a 'Cohort.Index' to an 'Cohort.AssessmentInterval'
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event ClaimsSchema c a) -- ^ interval predicate
  -> Predicate (Event ClaimsSchema c a) -- ^ a predicate on events
  -> Definition
       (  Feature indexName (i a)
       -> Feature eventsName (container (Event ClaimsSchema c a))
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
  -> Predicate (Event ClaimsSchema c a)
  -> Definition
       (  Feature indexName (i0 a)
       -> Feature eventsName (t (Event ClaimsSchema c a))
       -> Feature varName Binary
       )
buildNofXBinaryConcurBaseline n baselineDur =
  buildNofXBinary n (makeBaselineFromIndex baselineDur) concur
```

`buildNofXBinary` specialized to filter to events that concur with an assessment interval created by `makeBaselineFromIndex` of a specified duration and that have a given set of concepts.

```haskell
buildNofConceptsBinaryConcurBaseline
  :: (Intervallic i0 a, Witherable t, IntervalSizeable a b, Baseline i0 a, Ord c)
  => Natural -- ^ minimum number of events. 
  -> b  -- ^ duration of baseline (passed to 'Cohort.makeBaselineFromIndex')
  -> [c] -- ^ list of 'EventData.Concepts' passed to 'EventData.containsConcepts'
  -> Definition
       (  Feature indexName (i0 a)
       -> Feature eventsName (t (Event ClaimsSchema c a))
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
    , Interval Int -> AssessmentInterval Int
    , ComparativePredicateOf2 (AssessmentInterval Int) (Event ClaimsSchema Text Int)
    , Predicate (Event ClaimsSchema Text Int)
    )

type NofXTestCase
  = TestCase
      (F "index" (Interval Int), F "events" [Event ClaimsSchema Text Int])
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
  f = makeTestCaseOfIndexAndEvents
  g = makeEnrollmentEvent
  h = makeEventWithConcepts

buildNofXTests :: TestTree
buildNofXTests = makeTestGroup 
   "Tests of NofX template"
    (buildNofX id)
    buildNofXTestCases 

```
