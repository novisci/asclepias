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
  :: ( Intervallic i0
     , Intervallic i1
     , Witherable container0
     , Witherable container1
     )
  => (container0 (Event t m a) -> container1 (i1 a)) -- ^ function mapping a container of events to a container of intervallic intervals (which could be events!)
  -> (container1 (i1 a) -> t1) -- ^ function mapping the processed events to an intermediate type
  -> (AssessmentInterval a -> t1 -> outputType) -- ^ function casting intermediate type to output type with the option to use the assessment interval
  -> (i0 a -> AssessmentInterval a) -- ^ function which maps index interval to interval in which to assess the feature
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event t m a) -- ^ the interval relation of the input events to the assessment interval
  -> Predicate (Event t m a) -- ^ The predicate to filter to Enrollment events (e.g. 'FeatureEvents.isEnrollment')
  -> Definition
       (  Feature indexName (i0 a)
       -> Feature eventsName (container0 (Event t m a))
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
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event t m a)
  -> Predicate (Event t m a)
  -> Definition
       (  Feature indexName (Interval a)
       -> Feature eventsName [Event t m a]
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
       -> Feature eventsName [Event Text m a]
       -> Feature varName [b]
       )
{- tag::example1sig[] -}
{- tag::example1[] -}
defBaseline180Enrollment = example (makeBaselineMeetsIndex 180) -- <1>
                                   concur -- <2>
                                   (containsTag ["enrollment"]) -- <3>
{- end::example1[] -}
