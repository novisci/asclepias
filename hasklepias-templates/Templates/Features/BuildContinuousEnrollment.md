---
title: some-title
tags: [these, are tags]
---

## Description

```haskell top
module Templates.Features.BuildContinuousEnrollment
  ( buildContinuousEnrollment
  , buildContinuousEnrollmentTests
  ) where

import           Templates.FeatureReqs
```

The following example does blah...

```examples
f = buildContinuousEnrollment myMapper myPred 8 
```

## Definition

```haskell
buildContinuousEnrollment
  :: ( Monoid (container (Interval a))
    , Monoid (container (Maybe (Interval a)))
     , Applicative container
     , Witherable container
     , IntervalSizeable a b
     )
  => (Index i0 a -> AssessmentInterval a) -- ^ function which maps index interval to interval in which to assess enrollment
  -> Predicate (Event a)  -- ^ The predicate to filter to Enrollment events (e.g. 'FeatureEvents.isEnrollment')
  -> b  -- ^ duration of allowable gap between enrollment intervals
  -> Definition
       (  Feature indexName (Index i0 a)
       -> Feature eventsName (container (Event a))
       -> Feature prevName Status
       -> Feature varName Status
       )
buildContinuousEnrollment makeAssessmentInterval predicate allowableGap = define
  (\index events prevStatus -> case prevStatus of
    Exclude -> Exclude
    Include -> includeIf
      (allGapsWithinLessThanDuration
        allowableGap
        (makeAssessmentInterval index)
        (combineIntervals $ filter (getPredicate predicate) events)
      )
  )
```

## Examples

```haskell
type ContEnrollArgs
  = (Index Interval Int -> AssessmentInterval Int, Predicate (Event Int), Int)

type ContEnrollTestCase = TestCase
         ( F "index" (Index Interval Int)
         , F "events" [Event Int]
         , F "prev" Status
         )
         Status
         ContEnrollArgs

buildContinuousEnrollmentTestCases :: [ ContEnrollTestCase]
buildContinuousEnrollmentTestCases =
  [ f "Exclude if previously excluded" 
      (makeBaselineFromIndex 10, isEnrollmentEvent, 3) 
      (pure $ makeIndex $readIntervalSafe (0, 1), pure [], pure Exclude)
       Exclude
  , f "Exclude if no events" 
      (makeBaselineFromIndex 10, isEnrollmentEvent, 3) 
      (pure $ makeIndex $ readIntervalSafe (0, 1), pure [], pure Include)
      Exclude
  , f "Exclude if gap >= 3"
      (makeBaselineFromIndex 10, isEnrollmentEvent, 3)
      (pure $ makeIndex $ readIntervalSafe (10, 11), pure [g (1, 4), g (9, 12)], pure Include)
      Exclude
      {- 
                  -           <- Index
         ----------           <- Baseline
         ---     ---          <- Enrollment
        |--------------|
      -}
  , f "Exclude if gap >= 3" 
      (makeBaselineFromIndex 10, isEnrollmentEvent, 3)
      (pure $ makeIndex $ readIntervalSafe (10, 11), pure [g (1, 7)], pure  Include)
      Exclude
      {-
                  -           <- Index
        ----------            <- Baseline
         ------               <- Enrollment
        |--------------|
      -}
  , f "Exclude if gap >= 3"
     (makeBaselineFromIndex 10, isEnrollmentEvent, 3) 
     (pure $ makeIndex $ readIntervalSafe (10, 11), pure [g (6, 13)], pure Include)
     Exclude
        {-
                  -           <- Index
         ----------           <- Baseline
              -------         <- Enrollment
        |--------------|
      -}
  , f "Include if gaps less than 3"
      (makeBaselineFromIndex 10, isEnrollmentEvent, 3)
      (pure $ makeIndex $ readIntervalSafe (10, 11), pure [g (1, 3), g (5, 12)], pure Include)
      Include
      {-
                  -           <- Index
         ----------           <- Baseline
         --  -------          <- Enrollment
        |--------------|
      -}
  , f "Include if gaps less than 3"
      (makeBaselineFromIndex 10, isEnrollmentEvent, 3)
      (pure $ makeIndex $ readIntervalSafe (10, 11), pure [g (2, 9)], pure Include)
      Include
      {-
                  -           <- Index
         ----------           <- Baseline
          -------             <- Enrollment
        |--------------|
      -}
  , f "Include if gaps less than 3"
      (makeBaselineFromIndex 10, isEnrollmentEvent, 3)
      (pure $ makeIndex $ readIntervalSafe (10, 11), pure [g (1, 6), g (4, 8)], pure Include)
      Include
        {-
                  -           <- Index
         ----------           <- Baseline
         -----                <- Enrollment
             ----
        |--------------|
      -}
  ] where
  f = makeTestCase
  g = makeEnrollmentEvent

buildContinuousEnrollmentTests :: TestTree
buildContinuousEnrollmentTests = makeTestGroup 
   "Tests of continuous enrollment template"
    buildContinuousEnrollment 
    buildContinuousEnrollmentTestCases 
```