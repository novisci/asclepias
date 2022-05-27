module Templates.Features.BuildContinuousEnrollment
  ( buildContinuousEnrollment
  , buildContinuousEnrollmentTests
  ) where

import           Templates.FeatureReqs         as F
import           EventDataTheory               (makeGapsWithinPredicate)

{- | 
TODO
-}
{- tag::template0[] -}
buildContinuousEnrollment
  :: ( Monoid (container (Interval a))
     , Monoid (container (Maybe (Interval a)))
     , Applicative container
     , Witherable container
     , IntervalSizeable a b
     )
  => (i0 a -> AssessmentInterval a) -- ^ function which maps index interval to interval in which to assess enrollment
  -> Predicate (Event c m a)  -- ^ The predicate to filter to events (e.g. 'FeatureEvents.isEnrollment')
  -> b  -- ^ duration of allowable gap between intervals
  ->
    {- tag::templateDefSig0 [] -}
     Definition
       (  Feature indexName (i0 a)
       -> Feature eventsName (container (Event c m a))
       -> Feature prevName Status
       -> Feature varName Status
       )
    {- end::templateDefSig0 [] -}
buildContinuousEnrollment makeAssessmentInterval predicate allowableGap =
  define
    (\index events prevStatus -> case prevStatus of
      Exclude -> Exclude
      Include -> includeIf
        (makeGapsWithinPredicate
          all
          (<)
          allowableGap
          (makeAssessmentInterval index)
          (combineIntervals $ F.filterEvents predicate events)
        )
    )
{- end::template0[] -}


{- tag::example0[] -}
-- f = buildContinuousEnrollment myMapper myPred 8 
{- end::example0[] -}


type ContEnrollArgs
  = ( Interval Int -> AssessmentInterval Int
    , Predicate (Event Text TestSchema Int)
    , Int
    )

type ContEnrollTestCase
  = TestCase
      ( F "index" (Interval Int)
      , F "events" [Event Text TestSchema Int]
      , F "prev" Status
      )
      Status
      ContEnrollArgs

buildContinuousEnrollmentTestCases :: [ContEnrollTestCase]
buildContinuousEnrollmentTestCases =
  [ f "Exclude if previously excluded"
      (makeBaselineMeetsIndex 10     , isEnrollmentEvent, 3)
      (pure $ safeInterval (0, 1), pure []          , pure Exclude)
      Exclude
  , f "Exclude if no events"
      (makeBaselineMeetsIndex 10     , isEnrollmentEvent, 3)
      (pure $ safeInterval (0, 1), pure []          , pure Include)
      Exclude
  , f
    "Exclude if gap >= 3"
    (makeBaselineMeetsIndex 10       , isEnrollmentEvent         , 3)
    (pure $ safeInterval (10, 11), pure [g (1, 4), g (9, 12)], pure Include)
    Exclude
      {- 
                  -           <- Index
         ----------           <- Baseline
         ---     ---          <- Enrollment
        |--------------|
      -}
  , f "Exclude if gap >= 3"
      (makeBaselineMeetsIndex 10       , isEnrollmentEvent, 3)
      (pure $ safeInterval (10, 11), pure [g (1, 7)]  , pure Include)
      Exclude
      {-
                  -           <- Index
        ----------            <- Baseline
         ------               <- Enrollment
        |--------------|
      -}
  , f "Exclude if gap >= 3"
      (makeBaselineMeetsIndex 10       , isEnrollmentEvent, 3)
      (pure $ safeInterval (10, 11), pure [g (6, 13)] , pure Include)
      Exclude
        {-
                  -           <- Index
         ----------           <- Baseline
              -------         <- Enrollment
        |--------------|
      -}
  , f
    "Include if gaps less than 3"
    (makeBaselineMeetsIndex 10       , isEnrollmentEvent         , 3)
    (pure $ safeInterval (10, 11), pure [g (1, 3), g (5, 12)], pure Include)
    Include
      {-
                  -           <- Index
         ----------           <- Baseline
         --  -------          <- Enrollment
        |--------------|
      -}
  , f "Include if gaps less than 3"
      (makeBaselineMeetsIndex 10       , isEnrollmentEvent, 3)
      (pure $ safeInterval (10, 11), pure [g (2, 9)]  , pure Include)
      Include
      {-
                  -           <- Index
         ----------           <- Baseline
          -------             <- Enrollment
        |--------------|
      -}
  , f
    "Include if gaps less than 3"
    (makeBaselineMeetsIndex 10       , isEnrollmentEvent        , 3)
    (pure $ safeInterval (10, 11), pure [g (1, 6), g (4, 8)], pure Include)
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
