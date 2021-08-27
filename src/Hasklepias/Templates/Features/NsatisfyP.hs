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

module Hasklepias.Templates.Features.NsatisfyP
  ( buildNofX
  , buildNsatisfyPTests
  ) where

import           Cohort
import           EventData
import           Features
import           Hasklepias.Misc                ( F )
import           Hasklepias.FeatureEvents
import           Hasklepias.Templates.TestUtilities
import           Hasklepias.Reexports
import           Hasklepias.ReexportsUnsafe
import           Stype

-- | All the buildNSatisfyP tests.
buildNsatisfyPTests :: TestTree
buildNsatisfyPTests =
  testGroup
    "NsatisfyP"
    [ buildNofXTests
    , buildNofXWithGapTests ]

{-|
-}
buildNofXBase
  :: (Intervallic i0 a, Intervallic i1 a, Witherable container0, Witherable container1)
  => (container0 (Event a) -> container1 (i1 a)) -- ^ function mapping a container of events to a container of intervallic intervals (which could be events!)
  -> (container1 (i1 a) -> outputType) -- ^ function mapping the processed events to the output type
  -> (Index i0 a -> AssessmentInterval a) -- ^ function which maps index interval to interval in which to assess the feature
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a) -- ^ the interval relation of the input events to the assessment interval
  -> Predicate (Event a) -- ^ The predicate to filter to Enrollment events (e.g. 'FeatureEvents.isEnrollment')
  -> Definition
       (  Feature indexName (Index i0 a)
       -> Feature eventsName (container0 (Event a))
       -> Feature varName outputType
       )
buildNofXBase runProcess runPostProcess makeAssessmentInterval relation predicate = 
  define
    (\index ->
      -- filter events to those satisfying both
      -- the given relation to the assessment interval
      -- AND the given predicate
         filter (relation (makeAssessmentInterval index) &&& getPredicate predicate)
      -- run the processing function
      .> runProcess
      -- run the post processing function
      .> runPostProcess
    )

{-| Do N events relating to the 'AssessmentInterval' in some way the satisfy 
    the given predicate? 
-}
buildNofX
  :: (Intervallic i a, Witherable container)
  => Natural
  -> (Index i a -> AssessmentInterval a)
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a)
  -> Predicate (Event a)
  -> Definition
       (  Feature indexName (Index i a)
       -> Feature eventsName (container (Event a))
       -> Feature varName Bool
       )
buildNofX n = buildNofXBase id (\x -> length x >= naturalToInt n)

{-| Do N events relating to the 'AssessmentInterval' in some way the satisfy 
    the given predicate? 
-}
buildNofXBinary
  :: (Intervallic i a, Witherable t)
  => Natural
  -> (Index i a -> AssessmentInterval a)
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a)
  -> Predicate (Event a)
  -> Definition
       (  Feature indexName (Index i a)
       -> Feature eventsName (t (Event a))
       -> Feature varName Binary 
       )
buildNofXBinary n = buildNofXBase id (\x -> length x >= naturalToInt n |> fromBool)

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
  -> Bool
  -> TestCase
       (F "index" (Index Interval a), F "events" [Event a])
       Bool
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
    (\x -> testCase (getTestName x)
                    (makeAssertion x (uncurryN buildNofX (getBuilderArgs x)))
    )
    buildNofXTestCases
  )

{-| Do N events relating to the 'AssessmentInterval' in some way with at least
    a given gap between them the satisfy the given predicate? 
-}
buildNofXWithGap
  :: ( Intervallic i a
     , IntervalSizeable a b
     , IntervalCombinable i a
     , Witherable t
     )
  => Natural
  -> b
  -> (Index i a -> AssessmentInterval a)
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event a)
  -> Predicate (Event a)
  -> Definition
       (  Feature indexName (Index i a)
       -> Feature eventsName (t (Event a))
       -> Feature varName Bool
       )
buildNofXWithGap n allowableGap = buildNofXBase
  (-- just need the intervals  
      fmap getInterval
   -- pairGaps needs List input as the container type
   .> toList 
  )
  (-- get (Maybe) durations of interval gaps between all pairs
      pairGaps 
   -- throw away any non-gaps
   .> catMaybes 
   -- keep only those gap durations longer than the allowableGap
   .> filter (> allowableGap)
   -- are there at least as many cases as desired?
   .> \x -> length x >= naturalToInt n
  )


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
    , f "True if looking for (at least) no events and there are events satisfying gap condition"
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
   , f "False if no events and looking for 1"
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
  , f
    "False if 2 events but not satisfying gap condition"
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
  , f
    "True if 2 events satisfy gap condition"
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
  , f
    "True if 2 events satisfy gap condition "
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
  ] where
  f = makeTestInputs
  g = makeEnrollmentEvent
  h = makeEventWithConcepts

buildNofXWithGapTests :: TestTree
buildNofXWithGapTests = testGroup
  "Tests of NofXWithGap template"
  (fmap
    (\x -> testCase (getTestName x)
                    (makeAssertion x (uncurryN buildNofXWithGap (getBuilderArgs x)))
    )
    buildNofXWithGapTestCases
  )


{-| Do N events concurring with baseline of a given duration satisfy the given
    predicate? 
-}
buildNofXConcurBaseline
  :: (Intervallic i0 a, Witherable t, IntervalSizeable a b, Baseline i0 a)
  => Natural
  -> b
  -> Predicate (Event a)
  -> Definition
       (  Feature indexName (Index i0 a)
       -> Feature eventsName (t (Event a))
       -> Feature varName Bool
       )
buildNofXConcurBaseline n baselineDur =
  buildNofX n (makeBaselineFromIndex baselineDur) concur

{-| Do N events concurring with baseline of a given duration have a set of 
    concepts? 
-}
buildNofConceptsConcurBaseline
  :: (Intervallic i0 a, Witherable t, IntervalSizeable a b, Baseline i0 a)
  => Natural
  -> b
  -> [Text]
  -> Definition
       (  Feature indexName (Index i0 a)
       -> Feature eventsName (t (Event a))
       -> Feature varName Bool
       )
buildNofConceptsConcurBaseline n baselineDur cpts =
  buildNofX n (makeBaselineFromIndex baselineDur) concur (containsConcepts cpts)

