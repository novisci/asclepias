---
title: buildNofXWithGap
tags: []
---

## Description

 Are there N gaps of at least the given duration between any pair of events that relate to the assessment interval by the given relation and the satisfy the given predicate?

```haskell module
module Templates.Features.BuildNofXWithGap
  ( buildNofXWithGap
  , buildNofXWithGapBool
  , buildNofXWithGapBinary
  , buildNofXWithGapTests
  ) where

import           Data.Foldable                      ( toList )
import           Data.Maybe                         ( catMaybes )
import           Data.Text                          ( Text )
import           Flow                               ( (.>) )
import           GHC.Natural                        ( Natural, naturalToInt )
import           Test.Tasty                         ( TestTree )
import           Templates.FeatureReqs
import           Templates.Features.BuildNofXBase
import           Witherable                         ( Witherable )
```

## Usage

## Definition

```haskell
-- | Generate all pair-wise combinations of a single list.
pairs :: [a] -> [(a, a)]
-- copied from the hgeometry library (https://hackage.haskell.org/package/hgeometry-0.12.0.4/docs/src/Data.Geometry.Arrangement.Internal.html#allPairs)
-- TODO: better naming differences between pairs and allPairs?
-- TODO: generalize this function over more containers?
pairs = go
 where
  go []       = []
  go (x : xs) = fmap (x, ) xs <> go xs


-- | Gets the durations of gaps (via 'IntervalAlgebra.(><)') between all pairs
--   of the input.
pairGaps
  :: (Intervallic i a, IntervalSizeable a b, IntervalCombinable i a)
  => [i a]
  -> [Maybe b]
pairGaps es = fmap (fmap duration . uncurry (><)) (pairs es)

buildNofXWithGap
  :: ( Intervallic i a
     , IntervalSizeable a b
     , IntervalCombinable i a
     , Witherable container
     )
  => (Bool -> outputType)
  -> Natural -- ^ the minimum number of gaps
  -> b -- ^ the minimum duration of a gap
  -> (Index i a -> AssessmentInterval a)
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event ClaimsSchema c a)
  -> Predicate (Event ClaimsSchema c a)
  -> Definition
       (  Feature indexName (Index i a)
       -> Feature eventsName (container (Event ClaimsSchema c a))
       -> Feature varName outputType
       )
buildNofXWithGap cast nGaps allowableGap = buildNofXBase
  (-- just need the intervals  
     fmap getInterval
   -- pairGaps needs List input as the container type
    .> toList)
  (-- get (Maybe) durations of interval gaps between all pairs
    pairGaps
   -- throw away any non-gaps
  .> catMaybes
   -- keep only those gap durations at least the allowableGap
  .> filter (>= allowableGap)
   -- are there at least as many events as desired?
  .> \x -> length x >= naturalToInt nGaps
  )
  (const cast)
```

### Specialized Definitions

`buildNofXWithGap` specialized to return boolean.

```haskell
buildNofXWithGapBool
  :: ( Intervallic i a
     , IntervalSizeable a b
     , IntervalCombinable i a
     , Witherable container
     )
  => Natural -- ^ the minimum number of gaps
  -> b -- ^ the minimum duration of a gap
  -> (Index i a -> AssessmentInterval a)
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event ClaimsSchema c a)
  -> Predicate (Event ClaimsSchema c a)
  -> Definition
       (  Feature indexName (Index i a)
       -> Feature eventsName (container (Event ClaimsSchema c a))
       -> Feature varName Bool
       )
buildNofXWithGapBool = buildNofXWithGap id
```

`buildNofXWithGap` specialized to return `Binary`.

```haskell
buildNofXWithGapBinary
  :: ( Intervallic i a
     , IntervalSizeable a b
     , IntervalCombinable i a
     , Witherable container
     )
  => Natural -- ^ the minimum number of gaps
  -> b -- ^ the minimum duration of a gap
  -> (Index i a -> AssessmentInterval a)
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event ClaimsSchema c a)
  -> Predicate (Event ClaimsSchema c a)
  -> Definition
       (  Feature indexName (Index i a)
       -> Feature eventsName (container (Event ClaimsSchema c a))
       -> Feature varName Binary
       )
buildNofXWithGapBinary = buildNofXWithGap fromBool
```

## Examples

```haskell
type NofXWithGapArgs
  = ( Natural
    , Int
    , Index Interval Int -> AssessmentInterval Int
    , ComparativePredicateOf2 (AssessmentInterval Int) (Event ClaimsSchema Text Int)
    , Predicate (Event ClaimsSchema Text Int)
    )

type NofXWithGapTestCase
  = TestCase
      (F "index" (Index Interval Int), F "events" [Event ClaimsSchema Text Int])
      Bool
      NofXWithGapArgs

buildNofXWithGapTestCases :: [NofXWithGapTestCase]
buildNofXWithGapTestCases =
  [ f "True if looking for no events and there are no events"
      (0, 3, makeBaselineFromIndex 10, concur, isEnrollmentEvent)
      (10, 11)
      []
      True
      {-
                   -          <- Index
         ----------           <- Baseline
                              <- Enrollment
        |--------------|
      -}
  , f
    "True if looking for (at least) no events and there are events satisfying gap condition"
    (0, 3, makeBaselineFromIndex 10, concur, isEnrollmentEvent)
    (10, 11)
    [g (1, 2), g (8, 9)]
    True
      {-
                   -          <- Index
         ----------           <- Baseline
         -       -            <- Enrollment
        |--------------|
      -}
  , f "False if no events and looking for 1 gap"
      (1, 3, makeBaselineFromIndex 10, concur, isEnrollmentEvent)
      (10, 11)
      []
      False
      {-
                   -          <- Index
         ----------           <- Baseline
                              <- Enrollment
        |--------------|
      -}
  , f "False if a single event ClaimsSchema Text and looking for gap"
      (1, 3, makeBaselineFromIndex 10, concur, isEnrollmentEvent)
      (10, 11)
      [g (8, 9)]
      False
      {-
                   -          <- Index
         ----------           <- Baseline
                 -            <- Enrollment
        |--------------|
      -}
  , f "False if 1 gap but not satisfying gap condition"
      (1, 3, makeBaselineFromIndex 10, concur, isEnrollmentEvent)
      (10, 11)
      [g (6, 7), g (8, 9)]
      False
      {-
                   -          <- Index
         ----------           <- Baseline
               - -            <- Enrollment
        |--------------|
      -}
  , f "True if 1 gap satisfy gap condition"
      (1, 3, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
      (10, 11)
      [h ["C", "A"] (2, 3), h ["A", "B"] (8, 9)]
      True
      {-
                   -          <- Index
         ----------           <- Baseline
          -                   <- ["C", "A"]
                 -            <- ["A", "B"] 
        |--------------|
      -}
  , f "True if 1 gap satisfy gap condition "
      (1, 3, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
      (10, 11)
      [h ["C", "A"] (2, 3), h ["D", "E"] (5, 6), h ["A", "B"] (8, 9)]
      True
      {-
                   -          <- Index
         ----------           <- Baseline
          -                   <- ["C", "A"]
              -               <- ["D", "E"]
                 -            <- ["A", "B"] 
        |--------------|
      -}
  , f
    "True if 1 gap satisfy gap condition"
    (1, 3, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
    (10, 11)
    [ h ["A"] (1, 2)
    , h ["A"] (2, 3)
    , h ["A"] (3, 4)
    , h ["A"] (4, 5)
    , h ["A"] (5, 6)
    ]
    True
      {-
                    -          <- Index
          ----------           <- Baseline
          -                    <- ["A"]
           -                   <- ["A"]
            -                  <- ["A"]
             -                 <- ["A"]
              -                <- ["A"]
        |--------------|
      -}
  , f "False if no gap satisfy gap condition"
      (1, 3, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
      (10, 11)
      [h ["A"] (1, 2), h ["A"] (2, 3), h ["A"] (3, 4), h ["A"] (4, 5)]
      False
      {-
                    -          <- Index
          ----------           <- Baseline
          -                    <- ["A"]
           -                   <- ["A"]
            -                  <- ["A"]
             -                 <- ["A"]
        |--------------|
      -}
  ] where
  f = makeTestCaseOfIndexAndEvents
  g = makeEnrollmentEvent
  h = makeEventWithConcepts

buildNofXWithGapTests :: TestTree
buildNofXWithGapTests = makeTestGroup 
  "Tests of NofXWithGap template"
    (buildNofXWithGap id) 
    buildNofXWithGapTestCases 
```
