---
title: buildNofXBase
tags: []
---

## Description

The `buildNofXBase` template is generally used a basis for creating new templates that follow the pattern:

1. Filter events to those satisfying two conditions:
  * an interval relation with an assessment interval
  * a provided predicate function such as containing certain concepts
2. Preprocess these events.
3. Process the events.
4. Post-process the events, optionally in conjunction with the assessment interval

```haskell module
module Templates.Features.BuildNofXBase
  ( buildNofXBase 
  ) where

import           Templates.FeatureReqs
import           Data.Time                      ( Day )
import           Data.Text                      ( Text )
import           Witherable                     ( filter, Witherable )
import           Flow                           ( (.>) )
import qualified           EventDataTheory.Utilities      ( (&&&) )
```

```{r, echo = FALSE }
x <- 1
```

## Usage

The `exampleBuilder` function returns another feature builder that performs this logic:

1. combines the intervals of the input events (collapsing concurring and meeting intervals)
2. gets the `end` of each interval
3. computes the difference from each `end` to the `begin` of the assessment interval.

```haskell usage
buildExample :: 
    (Index Interval Day -> AssessmentInterval Day) 
  -> ComparativePredicateOf2 (AssessmentInterval Day) (Event ClaimsSchema Text Day) 
  -> Predicate (Event ClaimsSchema Text Day)
  -> Definition
       (  Feature indexName (Index Interval Day)
       -> Feature eventsName [Event ClaimsSchema Text Day]
       -> Feature varName [Integer]
       )
buildExample =
  buildNofXBase 
    combineIntervals 
    (fmap end)
    (\ai ends -> fmap (diff (begin ai)) ends)
```

To be fully specified as a `Definition` and used in a project, the `exampleBuilder` function needs 3 additional inputs:

1. a function mapping the index interval to an assessment interval.
2. a predicate function comparing events to the assessment interval.
3. another predicate function on the events.

For example, the `buildExampleBaselineEnrollment` is a `Definition` that performs the logic of `buildExample` on enrollment events concurring with baseline, where baseline is defined as 180 days prior to index.

```haskell usage
buildExampleBaseline180Enrollment = 
  buildExample
     (makeBaselineFromIndex 180)
     concur
     isEnrollmentEvent
```

## Definition

```haskell
buildNofXBase
  :: ( Intervallic i0 a
     , Intervallic i1 a
     , Witherable container0
     , Witherable container1
     )
  => (container0 (Event ClaimsSchema Text a) -> container1 (i1 a)) -- ^ function mapping a container of events to a container of intervallic intervals (which could be events!)
  -> (container1 (i1 a) -> t) -- ^ function mapping the processed events to an intermediate type
  -> (AssessmentInterval a -> t -> outputType) -- ^ function casting intermediate type to output type with the option to use the assessment interval
  -> (Index i0 a -> AssessmentInterval a) -- ^ function which maps index interval to interval in which to assess the feature
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event ClaimsSchema Text a) -- ^ the interval relation of the input events to the assessment interval
  -> Predicate (Event ClaimsSchema Text a) -- ^ The predicate to filter to Enrollment events (e.g. 'FeatureEvents.isEnrollment')
  -> Definition
       (  Feature indexName (Index i0 a)
       -> Feature eventsName (container0 (Event ClaimsSchema Text a))
       -> Feature varName outputType
       )
buildNofXBase runPreProcess runProcess runPostProcess makeAssessmentInterval relation predicate
  = define
    (\index ->
      -- filter events to those satisfying both
      -- the given relation to the assessment interval
      -- AND the given predicate
      Witherable.filter
          (getPredicate ((Predicate (relation (makeAssessmentInterval index))) &&& predicate))
      -- run the preprocessing function
        .> runPreProcess
      -- run the processing function
        .> runProcess
      -- run the postprocessing function
        .> runPostProcess (makeAssessmentInterval index)
    )
```

## Examples

This builder is used to build other builders and does not include any test or examples.
