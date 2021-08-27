{-|
Module      :  Features Templates 
Description : Templates for Features based on satisfying a set of predicates 
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module Hasklepias.Templates.Features.NsatisfyP
  ( buildNsatisfyPTests
  , buildNofX
  , buildNofXBool
  , buildNofXBinary
  , buildNofXBinaryConcurBaseline
  , buildNofConceptsBinaryConcurBaseline
  , buildNofXWithGap
  , buildNofXWithGapBool
  , buildNofXWithGapBinary
  , buildNofUniqueBegins
  ) where

import           Cohort
import qualified Control.Lens                  as Functor
import           EventData
import           Features
import           Hasklepias.FeatureEvents
import           Hasklepias.Misc                ( F )
import           Hasklepias.Reexports
import           Hasklepias.ReexportsUnsafe
import           Hasklepias.Templates.TestUtilities
import           Stype

-- | All the buildNSatisfyP tests.
buildNsatisfyPTests :: TestTree
buildNsatisfyPTests =
  testGroup "NsatisfyP" [buildNofXTests, buildNofXWithGapTests, buildNofUniqueBeginsTests]

{-|
-}
buildNofXBase
  :: ( Intervallic i0 a
     , Intervallic i1 a
     , Witherable container0
     , Witherable container1
     )
  => (container0 (Event a) -> container1 (i1 a)) -- ^ function mapping a container of events to a container of intervallic intervals (which could be events!)
  -> (container1 (i1 a) -> t) -- ^ function mapping the processed events to an intermediate type
  -> (AssessmentInterval a -> t -> outputType) -- ^ function casting intermediate type to output type with the option to use the assessment interval
  -> (Index i0 a -> AssessmentInterval a) -- ^ function which maps index interval to interval in which to assess the feature
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a) -- ^ the interval relation of the input events to the assessment interval
  -> Predicate (Event a) -- ^ The predicate to filter to Enrollment events (e.g. 'FeatureEvents.isEnrollment')
  -> Definition
       (  Feature indexName (Index i0 a)
       -> Feature eventsName (container0 (Event a))
       -> Feature varName outputType
       )
buildNofXBase runProcess runPostProcess runCast makeAssessmentInterval relation predicate
  = define
    (\index ->
      -- filter events to those satisfying both
      -- the given relation to the assessment interval
      -- AND the given predicate
      filter
          (relation (makeAssessmentInterval index) &&& getPredicate predicate)
      -- run the processing function
        .> runProcess
      -- run the post processing function
        .> runPostProcess
      -- run the casting function
        .> runCast (makeAssessmentInterval index)
    )

{-| Do N events relating to the 'AssessmentInterval' in some way the satisfy 
    the given predicate? 
-}
buildNofX
  :: (Intervallic i a, Witherable container)
  => (Bool -> outputType) -- ^ casting function
  -> Natural -- ^ minimum number of cases
  -> (Index i a -> AssessmentInterval a) -- ^ function to transform a 'Cohort.Index' to an 'Cohort.AssessmentInterval'
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a) -- ^ interval predicate
  -> Predicate (Event a) -- ^ a predicate on events
  -> Definition
       (  Feature indexName (Index i a)
       -> Feature eventsName (container (Event a))
       -> Feature varName outputType
       )
buildNofX f n = buildNofXBase id (\x -> length x >= naturalToInt n) (const f)

-- | 'buildNofX' specialized to return 'Bool'.
buildNofXBool
  :: (Intervallic i a, Witherable container)
  => Natural -- ^ minimum number of cases 
  -> (Index i a -> AssessmentInterval a) -- ^ function to transform a 'Cohort.Index' to an 'Cohort.AssessmentInterval'
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a) -- ^ interval predicate
  -> Predicate (Event a) -- ^ a predicate on events
  -> Definition
       (  Feature indexName (Index i a)
       -> Feature eventsName (container (Event a))
       -> Feature varName Bool
       )
buildNofXBool = buildNofX id

-- | 'buildNofX' specialized to return 'Stype.Binary'.
buildNofXBinary
  :: (Intervallic i a, Witherable container)
  => Natural
  -> (Index i a -> AssessmentInterval a)
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a)
  -> Predicate (Event a)
  -> Definition
       (  Feature indexName (Index i a)
       -> Feature eventsName (container (Event a))
       -> Feature varName Binary
       )
buildNofXBinary = buildNofX fromBool

{- | 
'buildNofXBinary' specialized to filter to events that 'IntervalAlgebra.concur' 
with an 'Cohort.AssessmentInterval' created by 'Cohort.makeBaselineFromIndex' of
a specified duration and a provided 'Data.Functor.Contravariant.Predicate'.
-}
buildNofXBinaryConcurBaseline
  :: (Intervallic i0 a, Witherable t, IntervalSizeable a b, Baseline i0 a)
  => Natural -- ^ minimum number of events.
  -> b -- ^ duration of baseline (passed to 'Cohort.makeBaselineFromIndex')
  -> Predicate (Event a)
  -> Definition
       (  Feature indexName (Index i0 a)
       -> Feature eventsName (t (Event a))
       -> Feature varName Binary
       )
buildNofXBinaryConcurBaseline n baselineDur =
  buildNofXBinary n (makeBaselineFromIndex baselineDur) concur

{- | 
'buildNofXBinary' specialized to filter to events that 'IntervalAlgebra.concur' 
with an 'Cohort.AssessmentInterval' created by 'Cohort.makeBaselineFromIndex' of
a specified duration and that have a given set of 'EventData.Concepts'.
-}
buildNofConceptsBinaryConcurBaseline
  :: (Intervallic i0 a, Witherable t, IntervalSizeable a b, Baseline i0 a)
  => Natural -- ^ minimum number of events. 
  -> b  -- ^ duration of baseline (passed to 'Cohort.makeBaselineFromIndex')
  -> [Text] -- ^ list of 'EventData.Concepts' passed to 'EventData.containsConcepts'
  -> Definition
       (  Feature indexName (Index i0 a)
       -> Feature eventsName (t (Event a))
       -> Feature varName Bool
       )
buildNofConceptsBinaryConcurBaseline n baselineDur cpts = buildNofXBool
  n
  (makeBaselineFromIndex baselineDur)
  concur
  (containsConcepts cpts)

--------------------------------------------------------------------------------
-- NofX examples/tests
--------------------------------------------------------------------------------

type NofXArgs
  = ( Natural
    , Index Interval Int -> AssessmentInterval Int
    , ComparativePredicateOf2 (AssessmentInterval Int) (Event Int)
    , Predicate (Event Int)
    )

makeTestInputs
  :: (Integral b, IntervalSizeable a b)
  => TestName
  -> bargs
  -> (a, a)
  -> [Event a]
  -> returnType 
  -> TestCase
       (F "index" (Index Interval a), F "events" [Event a])
       returnType
       bargs
makeTestInputs name buildArgs intrvl e b = MkTestCase
  buildArgs
  name
  (pure (makeIndex (readIntervalSafe intrvl)), pure e)
  (pure b)

type NofXTestCase
  = TestCase
      (F "index" (Index Interval Int), F "events" [Event Int])
      Bool
      NofXArgs

buildNofXTestCases :: [NofXTestCase]
buildNofXTestCases =
  [ f "False if no events"
      (1, makeBaselineFromIndex 10, concur, isEnrollmentEvent)
      (0, 1)
      []
      False
  , f
    "False if 1 event after index but looking for single event concurring with baseline"
    (1, makeBaselineFromIndex 10, concur, isEnrollmentEvent)
    (0, 1)
    [g (2, 7)]
    False
  , f
    "True if 1 event before index and looking for single event concurring with baseline"
    (1, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
    (0, 1)
    [h ["A", "B"] (-5, -4)]
    True
  , f
    "True if 2 events before index and looking for at least 2 events concurring with baseline"
    (2, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
    (0, 1)
    [h ["A", "B"] (-5, -4), h ["A", "C"] (-3, -2)]
    True
  , f
    "True if 3 events before index and looking for at least 2 events concurring with baseline"
    (2, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
    (0, 1)
    [h ["A", "B"] (-7, -6), h ["A", "B"] (-5, -4), h ["A", "C"] (-3, -2)]
    True
  , f
    "True if 2 events of same interval before index and looking for at least 2 events concurring with baseline"
    (2, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
    (0, 1)
    [h ["A"] (-5, -4), h ["A", "B"] (-5, -4)]
    True
  , f
    "False if 1 event before index and looking for at least 2 events concurring with baseline"
    (2, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
    (0, 1)
    [h ["A", "C"] (-3, -2)]
    False
  ] where
  f = makeTestInputs
  g = makeEnrollmentEvent
  h = makeEventWithConcepts

buildNofXTests :: TestTree
buildNofXTests = testGroup
  "Tests of NofX template"
  (fmap
    (\x -> testCase
      (getTestName x)
      (makeAssertion x (uncurryN buildNofXBool (getBuilderArgs x)))
    )
    buildNofXTestCases
  )

{-| Are there N gaps of at least the given duration between any pair of events 
    that relate to the 'AssessmentInterval' by the given relation and the
    satisfy the given predicate? 
-}
buildNofXWithGap
  :: ( Intervallic i a
     , IntervalSizeable a b
     , IntervalCombinable i a
     , Witherable container
     )
  => (Bool -> outputType)
  -> Natural -- ^ the minimum number of gaps
  -> b -- ^ the minimum duration of a gap
  -> (Index i a -> AssessmentInterval a)
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a)
  -> Predicate (Event a)
  -> Definition
       (  Feature indexName (Index i a)
       -> Feature eventsName (container (Event a))
       -> Feature varName outputType
       )
buildNofXWithGap cast nGaps allowableGap = buildNofXBase
  (-- just need the intervals  
     fmap getInterval
   -- pairGaps needs List input as the container type
    .> toList)
  (-- get (Maybe) durations of interval gaps between all pairs
    pairGaps
   -- throw away any non-gaps
  .> catMaybes
   -- keep only those gap durations at least the allowableGap
  .> filter (>= allowableGap)
   -- are there at least as many events as desired?
  .> \x -> length x >= naturalToInt nGaps
  )
  (const cast)

-- | 'buildNofXWithGap' specialized to return 'Bool'. 
buildNofXWithGapBool
  :: ( Intervallic i a
     , IntervalSizeable a b
     , IntervalCombinable i a
     , Witherable container
     )
  => Natural -- ^ the minimum number of gaps
  -> b -- ^ the minimum duration of a gap
  -> (Index i a -> AssessmentInterval a)
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a)
  -> Predicate (Event a)
  -> Definition
       (  Feature indexName (Index i a)
       -> Feature eventsName (container (Event a))
       -> Feature varName Bool
       )
buildNofXWithGapBool = buildNofXWithGap id


-- | 'buildNofXWithGap' specialized to return 'Stype.Binary'. 
buildNofXWithGapBinary
  :: ( Intervallic i a
     , IntervalSizeable a b
     , IntervalCombinable i a
     , Witherable container
     )
  => Natural -- ^ the minimum number of gaps
  -> b -- ^ the minimum duration of a gap
  -> (Index i a -> AssessmentInterval a)
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a)
  -> Predicate (Event a)
  -> Definition
       (  Feature indexName (Index i a)
       -> Feature eventsName (container (Event a))
       -> Feature varName Binary
       )
buildNofXWithGapBinary = buildNofXWithGap fromBool

type NofXWithGapArgs
  = ( Natural
    , Int
    , Index Interval Int -> AssessmentInterval Int
    , ComparativePredicateOf2 (AssessmentInterval Int) (Event Int)
    , Predicate (Event Int)
    )

type NofXWithGapTestCase
  = TestCase
      (F "index" (Index Interval Int), F "events" [Event Int])
      Bool
      NofXWithGapArgs

buildNofXWithGapTestCases :: [NofXWithGapTestCase]
buildNofXWithGapTestCases =
  [ f "True if looking for no events and there are no events"
      (0, 3, makeBaselineFromIndex 10, concur, isEnrollmentEvent)
      (10, 11)
      []
      True
      {-
                   -          <- Index
         ----------           <- Baseline
                              <- Enrollment
        |--------------|
      -}
  , f
    "True if looking for (at least) no events and there are events satisfying gap condition"
    (0, 3, makeBaselineFromIndex 10, concur, isEnrollmentEvent)
    (10, 11)
    [g (1, 2), g (8, 9)]
    True
      {-
                   -          <- Index
         ----------           <- Baseline
         -       -            <- Enrollment
        |--------------|
      -}
  , f "False if no events and looking for 1 gap"
      (1, 3, makeBaselineFromIndex 10, concur, isEnrollmentEvent)
      (10, 11)
      []
      False
      {-
                   -          <- Index
         ----------           <- Baseline
                              <- Enrollment
        |--------------|
      -}
  , f "False if a single event and looking for gap"
      (1, 3, makeBaselineFromIndex 10, concur, isEnrollmentEvent)
      (10, 11)
      [g (8, 9)]
      False
      {-
                   -          <- Index
         ----------           <- Baseline
                 -            <- Enrollment
        |--------------|
      -}
  , f "False if 1 gap but not satisfying gap condition"
      (1, 3, makeBaselineFromIndex 10, concur, isEnrollmentEvent)
      (10, 11)
      [g (6, 7), g (8, 9)]
      False
      {-
                   -          <- Index
         ----------           <- Baseline
               - -            <- Enrollment
        |--------------|
      -}
  , f "True if 1 gap satisfy gap condition"
      (1, 3, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
      (10, 11)
      [h ["C", "A"] (2, 3), h ["A", "B"] (8, 9)]
      True
      {-
                   -          <- Index
         ----------           <- Baseline
          -                   <- ["C", "A"]
                 -            <- ["A", "B"] 
        |--------------|
      -}
  , f "True if 1 gap satisfy gap condition "
      (1, 3, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
      (10, 11)
      [h ["C", "A"] (2, 3), h ["D", "E"] (5, 6), h ["A", "B"] (8, 9)]
      True
      {-
                   -          <- Index
         ----------           <- Baseline
          -                   <- ["C", "A"]
              -               <- ["D", "E"]
                 -            <- ["A", "B"] 
        |--------------|
      -}
  , f
    "True if 1 gap satisfy gap condition"
    (1, 3, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
    (10, 11)
    [ h ["A"] (1, 2)
    , h ["A"] (2, 3)
    , h ["A"] (3, 4)
    , h ["A"] (4, 5)
    , h ["A"] (5, 6)
    ]
    True
      {-
                    -          <- Index
          ----------           <- Baseline
          -                    <- ["A"]
           -                   <- ["A"]
            -                  <- ["A"]
             -                 <- ["A"]
              -                <- ["A"]
        |--------------|
      -}
  , f "False if no gap satisfy gap condition"
      (1, 3, makeBaselineFromIndex 10, concur, containsConcepts ["A"])
      (10, 11)
      [h ["A"] (1, 2), h ["A"] (2, 3), h ["A"] (3, 4), h ["A"] (4, 5)]
      False
      {-
                    -          <- Index
          ----------           <- Baseline
          -                    <- ["A"]
           -                   <- ["A"]
            -                  <- ["A"]
             -                 <- ["A"]
        |--------------|
      -}
  ] where
  f = makeTestInputs
  g = makeEnrollmentEvent
  h = makeEventWithConcepts

buildNofXWithGapTests :: TestTree
buildNofXWithGapTests = testGroup
  "Tests of NofXWithGap template"
  (fmap
    (\x -> testCase
      (getTestName x)
      (makeAssertion x (uncurryN buildNofXWithGapBool (getBuilderArgs x)))
    )
    buildNofXWithGapTestCases
  )


{-
-}

{-| Do N events relating to the 'AssessmentInterval' in some way the satisfy 
    the given predicate? 
-}
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
  f = makeTestInputs
  h = makeEventWithConcepts

buildNofUniqueBeginsTests :: TestTree
buildNofUniqueBeginsTests = testGroup
  "Tests ofNofUniqueBegins template"
  (fmap
    (\x -> testCase
      (getTestName x)
      (makeAssertion x (uncurryN buildNofUniqueBegins (getBuilderArgs x)))
    )
    buildNofUniqueBeginsTestCases
  )
