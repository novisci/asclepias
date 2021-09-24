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

makeContinuousEnrollmentTestInputs
  :: (Integral b, IntervalSizeable a b)
  => TestName
  -> ContEnrollArgs
  -> (a, a)
  -> [Event a]
  -> Status
  -> Status
  -> TestCase
       ( F "index" (Index Interval a)
       , F "events" [Event a]
       , F "prev" Status
       )
       Status
       ContEnrollArgs
makeContinuousEnrollmentTestInputs name buildArgs intrvl e prev s = MkTestCase
  buildArgs
  name
  (pure (makeIndex (readIntervalSafe intrvl)), pure e, pure prev)
  (pure s)

commonArgs
  :: (Index Interval Int -> AssessmentInterval Int, Predicate (Event a), Int)
commonArgs = (makeBaselineFromIndex 10, isEnrollmentEvent, 3)
```

```haskell
buildContinuousEnrollmentTestCases
  :: [ TestCase
         ( F "index" (Index Interval Int)
         , F "events" [Event Int]
         , F "prev" Status
         )
         Status
         ContEnrollArgs
     ]
buildContinuousEnrollmentTestCases =
  [ f "Exclude if previously excluded" commonArgs (0, 1) [] Exclude Exclude
  , f "Exclude if no events"           commonArgs (0, 1) [] Include Exclude
  , f "Exclude if gap >= 3"
      commonArgs
      (10, 11)
      [g (1, 4), g (9, 12)]
      Include
      Exclude
      {- 
                  -           <- Index
         ----------           <- Baseline
         ---     ---          <- Enrollment
        |--------------|
      -}
  , f "Exclude if gap >= 3" commonArgs (10, 11) [g (1, 7)]  Include Exclude
      {-
                  -           <- Index
        ----------            <- Baseline
         ------               <- Enrollment
        |--------------|
      -}
  , f "Exclude if gap >= 3" commonArgs (10, 11) [g (6, 13)] Include Exclude
        {-
                  -           <- Index
         ----------           <- Baseline
              -------         <- Enrollment
        |--------------|
      -}
  , f "Include if gaps less than 3"
      commonArgs
      (10, 11)
      [g (1, 3), g (5, 12)]
      Include
      Include
      {-
                  -           <- Index
         ----------           <- Baseline
         --  -------          <- Enrollment
        |--------------|
      -}
  , f "Include if gaps less than 3"
      commonArgs
      (10, 11)
      [g (2, 9)]
      Include
      Include
      {-
                  -           <- Index
         ----------           <- Baseline
          -------             <- Enrollment
        |--------------|
      -}
  , f "Include if gaps less than 3"
      commonArgs
      (10, 11)
      [g (1, 6), g (4, 8)]
      Include
      Include
        {-
                  -           <- Index
         ----------           <- Baseline
         -----                <- Enrollment
             ----
        |--------------|
      -}
  ] where
  f = makeContinuousEnrollmentTestInputs
  g = makeEnrollmentEvent

buildContinuousEnrollmentTests :: TestTree
buildContinuousEnrollmentTests = testGroup
  "Tests of continuous enrollment template"
  (fmap
    (\x -> testCase
      (getTestName x)
      (makeAssertion
        x
        (uncurryN $ eval (buildContinuousEnrollment (makeBaselineFromIndex 10) isEnrollmentEvent 3))
      )
    )
    buildContinuousEnrollmentTestCases
  )
```
