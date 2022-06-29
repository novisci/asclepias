module Templates.Features.BuildNofX
  ( buildNofX
  , buildNofXBool
  , buildNofXBinary
  , buildNofXBinaryConcurBaseline
  , buildNofTagSetBinaryConcurBaseline
  , buildNofXTests
  ) where

import           Templates.FeatureReqs
import           Templates.Features.BuildNofXBase

{- tag::template0[] -}
buildNofX
  :: (Intervallic i a, Witherable container)
  => (Bool -> outputType) -- ^ casting function
  -> Natural -- ^ minimum number of cases
  -> (i a -> AssessmentInterval a) -- ^ function to transform a 'Cohort.Index' to an 'Cohort.AssessmentInterval'
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event t m a) -- ^ interval predicate
  -> Predicate (Event t m a) -- ^ a predicate on events
  -> Definition
       (  Feature indexName (i a)
       -> Feature eventsName (container (Event t m a))
       -> Feature varName outputType
       )
buildNofX f n = buildNofXBase id (\x -> length x >= naturalToInt n) (const f)
{- end::template0[] -}

{- tag::template1[] -}
buildNofXBinary
  :: (Intervallic i a, Witherable container)
  => Natural
  -> (i a -> AssessmentInterval a)
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event t m a)
  -> Predicate (Event t m a)
  -> Definition
       (  Feature indexName (i a)
       -> Feature eventsName (container (Event t m a))
       -> Feature varName Binary
       )
buildNofXBinary = buildNofX fromBool
{- end::template1[] -}

{- tag::template2[] -}
buildNofXBool
  :: (Intervallic i a, Witherable container)
  => Natural -- ^ minimum number of cases 
  -> (i a -> AssessmentInterval a) -- ^ function to transform a to an 'AssessmentInterval'
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event t m a) -- ^ interval predicate
  -> Predicate (Event t m a) -- ^ a predicate on events
  -> Definition
       (  Feature indexName (i a)
       -> Feature eventsName (container (Event t m a))
       -> Feature varName Bool
       )
buildNofXBool = buildNofX id
{- end::template2[] -}

{- tag::template3[] -}
buildNofXBinaryConcurBaseline
  :: (Intervallic i0 a, Witherable t1, IntervalSizeable a b, Baseline i0 a)
  => Natural -- ^ minimum number of events.
  -> b -- ^ duration of baseline (passed to 'makeBaselineMeetsIndex')
  -> Predicate (Event t m a)
  -> Definition
       (  Feature indexName (i0 a)
       -> Feature eventsName (t1 (Event t m a))
       -> Feature varName Binary
       )
buildNofXBinaryConcurBaseline n baselineDur =
  buildNofXBinary n (makeBaselineMeetsIndex baselineDur) concur
{- end::template3[] -}

{- tag::template4[] -}
buildNofTagSetBinaryConcurBaseline
  :: ( Intervallic i0 a
     , Witherable t1
     , IntervalSizeable a b
     , Baseline i0 a
     , Ord t
     )
  => Natural -- ^ minimum number of events. 
  -> b  -- ^ duration of baseline (passed to 'Cohort.makeBaselineMeetsIndex')
  -> [t] -- ^ list of 'EventData.Tag' passed to 'EventData.containsTag'
  -> Definition
       (  Feature indexName (i0 a)
       -> Feature eventsName (t1 (Event t m a))
       -> Feature varName Binary
       )
buildNofTagSetBinaryConcurBaseline n baselineDur tagSet = buildNofXBinary
  n
  (makeBaselineMeetsIndex baselineDur)
  concur
  (containsTag tagSet)
{- tag::template4[] -}

type NofXArgs
  = ( Natural
    , Interval Int -> AssessmentInterval Int
    , ComparativePredicateOf2
        (AssessmentInterval Int)
        (Event Text TestSchema Int)
    , Predicate (Event Text TestSchema Int)
    )

type NofXTestCase
  = TestCase
      (F "index" (Interval Int), F "events" [Event Text TestSchema Int])
      Bool
      NofXArgs

buildNofXTestCases :: [NofXTestCase]
buildNofXTestCases =
  [ f "False if no events"
      (1, makeBaselineMeetsIndex 10, concur, isEnrollmentEvent)
      (0, 1)
      []
      False
  , f
    "False if 1 event after index but looking for single event concurring with baseline"
    (1, makeBaselineMeetsIndex 10, concur, isEnrollmentEvent)
    (0, 1)
    [g (2, 7)]
    False
  , f
    "True if 1 event before index and looking for single event concurring with baseline"
    (1, makeBaselineMeetsIndex 10, concur, containsTag ["A"])
    (0, 1)
    [h ["A", "B"] (-5, -4)]
    True
  , f
    "True if 2 events before index and looking for at least 2 events concurring with baseline"
    (2, makeBaselineMeetsIndex 10, concur, containsTag ["A"])
    (0, 1)
    [h ["A", "B"] (-5, -4), h ["A", "C"] (-3, -2)]
    True
  , f
    "True if 3 events before index and looking for at least 2 events concurring with baseline"
    (2, makeBaselineMeetsIndex 10, concur, containsTag ["A"])
    (0, 1)
    [h ["A", "B"] (-7, -6), h ["A", "B"] (-5, -4), h ["A", "C"] (-3, -2)]
    True
  , f
    "True if 2 events of same interval before index and looking for at least 2 events concurring with baseline"
    (2, makeBaselineMeetsIndex 10, concur, containsTag ["A"])
    (0, 1)
    [h ["A"] (-5, -4), h ["A", "B"] (-5, -4)]
    True
  , f
    "False if 1 event before index and looking for at least 2 events concurring with baseline"
    (2, makeBaselineMeetsIndex 10, concur, containsTag ["A"])
    (0, 1)
    [h ["A", "C"] (-3, -2)]
    False
  ] where
  f = makeTestCaseOfIndexAndEvents
  g = makeEnrollmentEvent
  h = makeEventWithTagSet

buildNofXTests :: TestTree
buildNofXTests =
  makeTestGroup "Tests of NofX template" (buildNofX id) buildNofXTestCases

