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
type IsEnrolledArgs
  = ( OneTuple (Predicate (Event Int ) )) 
  -- use of OneTuple is because buildIsEnrolled takes a single argument and the
  -- OneTuple is needed to match the Curry constraints in the makeBuilderAssertion
  -- (via makeTestGroup). In the buildIsEnrolledTestCases, the OneTuple by its
  -- Applicative instance using `pure`. 

type IsEnrolledTestCase = 
  TestCase
         (F "index" (Index Interval Int), F "events" [Event Int])
         Status
         IsEnrolledArgs
```

```haskell
buildIsEnrolledTestCases :: [ IsEnrolledTestCase ]
buildIsEnrolledTestCases =
  [ f "Exclude if no events" 
     ( pure isEnrollmentEvent )
     (0, 1) 
     [] 
     Exclude
  , f "Exclude if only interval meets"
      ( pure isEnrollmentEvent )
      (0, 1)
      [g (1, 6)]
      Exclude
  , f "Include if concurring interval"
      ( pure isEnrollmentEvent )
      (0, 1)
      [g (-1, 4)]
      Include
  , f "Include if concurring interval"
      ( pure isEnrollmentEvent )
      (0, 1)
      [g (-1, 1), g (1, 4)]
      Include
  ] where
  f = makeTestCaseOfIndexAndEvents
  g = makeEnrollmentEvent


buildIsEnrolledTests :: TestTree
buildIsEnrolledTests = makeTestGroup 
   "Tests of isEnrolled template"
    buildIsEnrolled
    buildIsEnrolledTestCases 
```
