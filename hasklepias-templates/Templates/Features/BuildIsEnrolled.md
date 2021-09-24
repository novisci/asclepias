---
title: some-title
tags: [these, are tags]
---

## Description

The is Enrolled template

```haskell
module Templates.Features.BuildIsEnrolled
  ( buildIsEnrolled
  , buildIsEnrolledTests
  ) where

import           Templates.FeatureReqs
```

## Definition

```haskell
buildIsEnrolled
  :: ( Intervallic i0 a
     , Monoid (container (Interval a))
     , Applicative container
     , Witherable container
     )
  =>
  Predicate (Event a) -- ^ The predicate to filter to Enrollment events (e.g. 'FeatureEvents.isEnrollment')
  -> Definition
       (  Feature indexName (Index i0 a)
       -> Feature eventsName (container (Event a))
       -> Feature varName Status
       )
buildIsEnrolled predicate = define
  (\index ->
    filter (getPredicate predicate)
      .> combineIntervals
      .> any (concur index)
      .> includeIf
  )
```

## Examples

```haskell
makeIsEnrolledTestInputs
  :: (Integral b, IntervalSizeable a b)
  => TestName
  -> Predicate (Event a)
  -> (a, a)
  -> [Event a]
  -> Status
  -> TestCase
       (F "index" (Index Interval a), F "events" [Event a])
       Status
       (Predicate (Event a))
makeIsEnrolledTestInputs name buildArgs intrvl e s = MkTestCase
  buildArgs
  name
  (pure (makeIndex $ readIntervalSafe intrvl), pure e)
  (pure s)
```

```haskell
buildIsEnrolledTestCases
  :: [ TestCase
         (F "index" (Index Interval Int), F "events" [Event Int])
         Status
         (Predicate (Event Int))
     ]
buildIsEnrolledTestCases =
  [ f "Exclude if no events" isEnrollmentEvent (0, 1) [] Exclude
  , f "Exclude if only interval meets"
      isEnrollmentEvent
      (0, 1)
      [g (1, 6)]
      Exclude
  , f "Include if concurring interval"
      isEnrollmentEvent
      (0, 1)
      [g (-1, 4)]
      Include
  , f "Include if concurring interval"
      isEnrollmentEvent
      (0, 1)
      [g (-1, 1), g (1, 4)]
      Include
  ] where
  f = makeIsEnrolledTestInputs
  g = makeEnrollmentEvent

buildIsEnrolledTests :: TestTree
buildIsEnrolledTests = testGroup
  "Tests of isEnrolled template"
  (fmap
    (\x -> testCase (getTestName x)
                    (makeAssertion x (uncurryN $ eval (buildIsEnrolled (getBuilderArgs x))))
    )
    buildIsEnrolledTestCases
  )
```
