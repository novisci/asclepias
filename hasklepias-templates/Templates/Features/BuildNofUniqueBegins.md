---
title: buildNofUniqueBegins
tags: []
---

## Description

Do N events relating to the 'AssessmentInterval' in some way the satisfy the given predicate?

```haskell module
module Templates.Features.BuildNofUniqueBegins
  ( buildNofUniqueBegins
  , buildNofUniqueBeginsTests
  ) where

import           Templates.FeatureReqs
import           Templates.Features.BuildNofXBase
```

## Usage

TODO

## Definition

```haskell
buildNofUniqueBegins
  :: (Intervallic i a, IntervalSizeable a b, Witherable container)
  => (Index i a -> AssessmentInterval a) -- ^ function to transform a 'Cohort.Index' to an 'Cohort.AssessmentInterval'
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a) -- ^ interval predicate
  -> Predicate (Event a) -- ^ a predicate on events
  -> Definition
       (  Feature indexName (Index i a)
       -> Feature eventsName (container (Event a))
       -> Feature varName [(EventTime b, Count)]
       )
buildNofUniqueBegins = buildNofXBase 
  ( fmap (momentize . getInterval) )
  (  fmap (, 1 :: Natural)
  .> toList
  .> mapFromList
  .> mapToList
  .> \x -> uncurry zip (fmap (scanl1 (+)) (unzip x)) 
  ) 
  (\window ->
    fmap (\i -> (mkEventTime $ Just (diff (begin (fst i)) (begin window)), Count (snd i)))  
  )
```

## Examples

```haskell

type NofUniqueBeginsArgs
  = ( Index Interval Int -> AssessmentInterval Int
    , ComparativePredicateOf2 (AssessmentInterval Int) (Event Int)
    , Predicate (Event Int)
    )

type NofUniqueBeginsTestCase
  = TestCase
      (F "index" (Index Interval Int), F "events" [Event Int])
      [(EventTime Int, Count)]
      NofUniqueBeginsArgs

buildNofUniqueBeginsTestCases :: [NofUniqueBeginsTestCase]
buildNofUniqueBeginsTestCases =
  [ f "empty input"
      (makeFollowupFromIndex 10, concur, isEnrollmentEvent)
      (0, 1)
      []
      [] 
      {-
         -                    <- Index
         ----------           <- Baseline

        |--------------|
      -}
  , f "2 results if 2 different begins"
      (makeFollowupFromIndex 10, concur, containsConcepts ["A"])
      (0, 1)
      [h ["A"] (2, 5), h ["A"] (4, 5)]
      [(mkEventTime (Just 2), 1), (mkEventTime (Just 4), 2)] 
      {-
         -                    <- Index
         ----------           <- Followup
           ---                <- "A"
             _                <- "A"
        |--------------|
      -}
  , f "2 results when multiple begins at same time"
      (makeFollowupFromIndex 10, concur, containsConcepts ["A"])
      (0, 1)
      [h ["A"] (2, 3),h ["A"] (2, 5), h ["A"] (4, 5)]
      [(mkEventTime (Just 2), 1), (mkEventTime (Just 4), 2)] 
      {-
         -                    <- Index
         ----------           <- Followup 
           -                  <- "A"
           ---                <- "A"
             -                <- "A"
        |--------------|
      -}
  , f "1 result based on predicate filter"
      (makeFollowupFromIndex 10, concur, containsConcepts ["A"])
      (0, 1)
      [h ["B"] (2, 3),h ["B"] (2, 5), h ["A"] (4, 5)]
      [(mkEventTime (Just 4), 1)] 
      {-
         -                    <- Index
         ----------           <- Followup 
           -                  <- "B"
           ---                <- "B"
             -                <- "A"
        |--------------|
      -}
  ] where
  f = makeTestCaseOfIndexAndEvents
  h = makeEventWithConcepts

buildNofUniqueBeginsTests :: TestTree
buildNofUniqueBeginsTests = makeTestGroup 
   "Tests ofNofUniqueBegins template"
    buildNofUniqueBegins
    buildNofUniqueBeginsTestCases 

```
