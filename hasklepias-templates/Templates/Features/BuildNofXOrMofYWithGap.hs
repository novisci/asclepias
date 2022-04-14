module Templates.Features.BuildNofXOrMofYWithGap
  ( buildNofXOrMofYWithGap
  , buildNofXOrMofYWithGapBool
  , buildNofXOrMofYWithGapBinary
  , buildNofXOrMofYWithGapTests
  ) where

import           Templates.FeatureReqs
import           Templates.Features.BuildNofX
import           Templates.Features.BuildNofXWithGap

{- tag::template0[] -}
buildNofXOrMofYWithGap
  :: ( Intervallic i a
     , IntervalSizeable a b
     , IntervalCombinable i a
     , Witherable container
     )
  => (outputType -> outputType -> outputType)
  -> (Bool -> outputType)
  -> Natural -- ^ count passed to 'buildNofX'
  -> Predicate (Event c m a)
  -> Natural -- ^ the minimum number of gaps passed to 'buildNofXWithGap'
  -> b -- ^ the minimum duration of a gap passed to 'buildNofXWithGap'
  -> (i a -> AssessmentInterval a)
  -> ComparativePredicateOf2
       (AssessmentInterval a)
       (Event c m a)
  -> Predicate (Event c m a)
  -> Definition
       (  Feature indexName (i a)
       -> Feature
            eventsName
            (container (Event c m a))
       -> Feature varName outputType
       )
buildNofXOrMofYWithGap f cast xCount xPred gapCount gapDuration assess intervalPred yPred
  = D2C f
        (buildNofX cast xCount assess intervalPred xPred)
        (buildNofXWithGap cast gapCount gapDuration assess intervalPred yPred)
{- end::template0[] -}

{- tag::template1[] -}
buildNofXOrMofYWithGapBool
  :: ( Intervallic i a
     , IntervalSizeable a b
     , IntervalCombinable i a
     , Witherable container
     )
  => Natural -- ^ count passed to 'buildNofX'
  -> Predicate (Event c m a)
  -> Natural -- ^ the minimum number of gaps passed to 'buildNofXWithGap'
  -> b -- ^ the minimum duration of a gap passed to 'buildNofXWithGap'
  -> (i a -> AssessmentInterval a)
  -> ComparativePredicateOf2
       (AssessmentInterval a)
       (Event c m a)
  -> Predicate (Event c m a)
  -> Definition
       (  Feature indexName (i a)
       -> Feature eventsName (container (Event c m a))
       -> Feature varName Bool
       )
buildNofXOrMofYWithGapBool = buildNofXOrMofYWithGap (||) id
{- end::template1[] -}

{- tag::template2[] -}
buildNofXOrMofYWithGapBinary
  :: ( Intervallic i a
     , IntervalSizeable a b
     , IntervalCombinable i a
     , Witherable container
     )
  => Natural -- ^ count passed to 'buildNofX'
  -> Predicate (Event c m a)
  -> Natural -- ^ the minimum number of gaps passed to 'buildNofXWithGap'
  -> b -- ^ the minimum duration of a gap passed to 'buildNofXWithGap'
  -> (i a -> AssessmentInterval a)
  -> ComparativePredicateOf2
       (AssessmentInterval a)
       (Event c m a)
  -> Predicate (Event c m a)
  -> Definition
       (  Feature indexName (i a)
       -> Feature eventsName (container (Event c m a))
       -> Feature varName Binary
       )
buildNofXOrMofYWithGapBinary = buildNofXOrMofYWithGap
  (\x y -> fromBool $ (||) (toBool x) (toBool y))
  fromBool
{- tag::template2[] -}


type NofXOrMofYWithGapArgs
  = ( Natural
    , Predicate (Event Text TestSchema Int)
    , Natural
    , Int
    , Interval Int -> AssessmentInterval Int
    , ComparativePredicateOf2
        (AssessmentInterval Int)
        (Event Text TestSchema Int)
    , Predicate (Event Text TestSchema Int)
    )

type NofXOrMofYWithGapTestCase
  = TestCase
      (F "index" (Interval Int), F "events" [Event Text TestSchema Int])
      Bool
      NofXOrMofYWithGapArgs

buildNofXOrMofYWithGapTestCases :: [NofXOrMofYWithGapTestCase]
buildNofXOrMofYWithGapTestCases =
  [ f
    "True if looking for no events and there are no events"
    ( 0
    , containsConcepts ["A"]
    , 0
    , 3
    , makeBaselineFromIndex 10
    , concur
    , isEnrollmentEvent
    )
    (10, 11)
    []
    True
      {-
                   -          <- Index
         ----------           <- Baseline
                              
        |--------------|
      -}
  , f
    "True if looking for (at least) no events and there are events satisfying gap condition"
    ( 0
    , containsConcepts ["A"]
    , 0
    , 3
    , makeBaselineFromIndex 10
    , concur
    , isEnrollmentEvent
    )
    (10, 11)
    [g (1, 2), g (8, 9)]
    True
      {-
                   -          <- Index
         ----------           <- Baseline
         -       -            <- Enrollment
        |--------------|
      -}
  , f
    "False if no X or Y events and looking for 1 X or 1 Y gap"
    ( 1
    , containsConcepts ["A"]
    , 1
    , 3
    , makeBaselineFromIndex 10
    , concur
    , isEnrollmentEvent
    )
    (10, 11)
    []
    False
      {-
                   -          <- Index
         ----------           <- Baseline
                              <- Enrollment
        |--------------|
      -}
  , f
    "False if a no X and Y single event TestSchema Text  and looking for gap"
    ( 1
    , containsConcepts ["A"]
    , 1
    , 3
    , makeBaselineFromIndex 10
    , concur
    , isEnrollmentEvent
    )
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
    "False if no X 1 gap but not satisfying gap condition"
    ( 1
    , containsConcepts ["A"]
    , 1
    , 3
    , makeBaselineFromIndex 10
    , concur
    , isEnrollmentEvent
    )
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
    "True if 1 gap satisfy gap condition"
    ( 1
    , containsConcepts ["D"]
    , 1
    , 3
    , makeBaselineFromIndex 10
    , concur
    , containsConcepts ["A"]
    )
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
    "True if 1 X event"
    ( 1
    , containsConcepts ["C"]
    , 1
    , 3
    , makeBaselineFromIndex 10
    , concur
    , containsConcepts ["A"]
    )
    (10, 11)
    [h ["C", "A"] (2, 3)]
    True
      {-
                   -          <- Index
         ----------           <- Baseline
          -                   <- ["C", "A"]
        |--------------|
      -}
  , f
    "True if 1 gap satisfy gap condition "
    ( 2
    , containsConcepts ["D"]
    , 1
    , 3
    , makeBaselineFromIndex 10
    , concur
    , containsConcepts ["A"]
    )
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
    "False if only one X and if no gap satisfy gap condition "
    ( 2
    , containsConcepts ["D"]
    , 1
    , 3
    , makeBaselineFromIndex 10
    , concur
    , containsConcepts ["A"]
    )
    (10, 11)
    [h ["C", "A"] (2, 3), h ["D", "A"] (4, 5)]
    False
      {-
                   -          <- Index
         ----------           <- Baseline
          -                   <- ["C", "A"]
            -                 <- ["D", "A"]
        |--------------|
      -}
  , f
    "True if two X and if no gap satisfy gap condition "
    ( 2
    , containsConcepts ["D"]
    , 1
    , 3
    , makeBaselineFromIndex 10
    , concur
    , containsConcepts ["A"]
    )
    (10, 11)
    [h ["D", "A"] (2, 3), h ["D", "A"] (4, 5)]
    True
      {-
                   -          <- Index
         ----------           <- Baseline
          -                   <- ["D", "A"]
            -                 <- ["D", "A"]
        |--------------|
      -}
  ] where
  f = makeTestCaseOfIndexAndEvents
  g = makeEnrollmentEvent
  h = makeEventWithConcepts

buildNofXOrMofYWithGapTests :: TestTree
buildNofXOrMofYWithGapTests = makeTestGroup "Tests of NofXWithGap template"
                                            (buildNofXOrMofYWithGap (||) id)
                                            buildNofXOrMofYWithGapTestCases

