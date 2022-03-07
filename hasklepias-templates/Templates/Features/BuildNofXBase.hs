module Templates.Features.BuildNofXBase
  ( buildNofXBase
  ) where

import           EventDataTheory.Utilities      ( (&&&) )
import           Templates.FeatureReqs


{- |
The basis of definition templates that answer a question about
N events satisying a predicate X. 
-}
{- tag::template0[] -}
buildNofXBase
  :: ( Intervallic i0 a
     , Intervallic i1 a
     , Witherable container0
     , Witherable container1
     )
  => (container0 (Event d c a) -> container1 (i1 a)) -- ^ function mapping a container of events to a container of intervallic intervals (which could be events!)
  -> (container1 (i1 a) -> t) -- ^ function mapping the processed events to an intermediate type
  -> (AssessmentInterval a -> t -> outputType) -- ^ function casting intermediate type to output type with the option to use the assessment interval
  -> (i0 a -> AssessmentInterval a) -- ^ function which maps index interval to interval in which to assess the feature
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event d c a) -- ^ the interval relation of the input events to the assessment interval
  -> Predicate (Event d c a) -- ^ The predicate to filter to Enrollment events (e.g. 'FeatureEvents.isEnrollment')
  -> Definition
       (  Feature indexName (i0 a)
       -> Feature eventsName (container0 (Event d c a))
       -> Feature varName outputType
       )
buildNofXBase runPreProcess runProcess runPostProcess makeAssessmentInterval relation predicate
  = define
    (\index ->
      -- filter events to those satisfying both
      -- the given relation to the assessment interval
      -- AND the given predicate
      filterEvents
          (Predicate (relation (makeAssessmentInterval index)) &&& predicate)
      -- run the preprocessing function
        .> runPreProcess
      -- run the processing function
        .> runProcess
      -- run the postprocessing function
        .> runPostProcess (makeAssessmentInterval index)
    )
{- end::template0[] -}

{-
An example of using the buildNofXBase function
-}
{- tag::example0sig[] -}
example
  :: (Ord a, IntervalSizeable a b)
  => (Interval a -> AssessmentInterval a)
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event d c a)
  -> Predicate (Event d c a)
  -> Definition
       (  Feature indexName (Interval a)
       -> Feature eventsName [Event d c a]
       -> Feature varName [b]
       )
{- tag::example0sig[] -}
{- tag::example0[] -}
example = buildNofXBase combineIntervals -- <1>
                        (fmap end) -- <2>
                        (fmap . diff . begin) -- <3>
{- end::example0[] -}

{- tag::example1sig[] -}
defBaseline180Enrollment
  :: IntervalSizeable a b
  => Definition
       (  Feature indexName (Interval a)
       -> Feature eventsName [Event d Text a]
       -> Feature varName [b]
       )
{- tag::example1sig[] -}
{- tag::example1[] -}
defBaseline180Enrollment = example (makeBaselineFromIndex 180) -- <1>
                                   concur -- <2>
                                   (containsConcepts ["enrollment"]) -- <3>
{- end::example1[] -}
