module Templates.Features.BuildNofXBase
  ( buildNofXBase
  ) where

import           EventDataTheory.Utilities      ( (&&&) )
import           Templates.FeatureReqs


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

{- tag::example0[] -}
buildExample
  :: (Interval Day -> AssessmentInterval Day)
  -> ComparativePredicateOf2 (AssessmentInterval Day) (Event d c Day)
  -> Predicate (Event d c Day)
  -> Definition
       (  Feature indexName (Interval Day)
       -> Feature eventsName [Event d c Day]
       -> Feature varName [Integer]
       )
buildExample = buildNofXBase combineIntervals (fmap end) (fmap . diff . begin)
{- end::example0[] -}
