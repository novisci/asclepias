module Templates.Features.BuildNofUniqueBegins
  ( buildNofUniqueBegins
  , buildNofUniqueBeginsTests
  ) where

import           Data.Map.Strict               as M
                                                ( Map
                                                , fromList
                                                , toList
                                                )
import           Templates.FeatureReqs         as F
import           Templates.Features.BuildNofXBase

{- tag::template0[] -}
buildNofUniqueBegins
  :: (Intervallic i a, IntervalSizeable a b, Witherable container)
  => (i a -> AssessmentInterval a) -- ^ function to transform a 'Cohort.Index' to an 'Cohort.AssessmentInterval'
  -> ComparativePredicateOf2 (AssessmentInterval a) (Event c m a) -- ^ interval predicate
  -> Predicate (Event c m a) -- ^ a predicate on events
  -> Definition
       (  Feature indexName (i a)
       -> Feature eventsName (container (Event c m a))
       -> Feature varName [(b, Natural)]
       )
buildNofUniqueBegins = buildNofXBase
  (fmap (momentize . getInterval))
  (fmap (, 1 :: Natural) .> F.toList .> M.fromList .> M.toList .> \x ->
    uncurry zip (fmap (scanl1 (+)) (unzip x))
  )
  (\window -> fmap (\i -> (diff (begin (fst i)) (begin window), snd i)))
{- end::template0[] -}

type NofUniqueBeginsArgs
  = ( Interval Int -> AssessmentInterval Int
    , ComparativePredicateOf2
        (AssessmentInterval Int)
        (Event Text TestSchema Int)
    , Predicate (Event Text TestSchema Int)
    )

type NofUniqueBeginsTestCase
  = TestCase
      (F "index" (Interval Int), F "events" [Event Text TestSchema Int])
      [(Int, Natural)]
      NofUniqueBeginsArgs

buildNofUniqueBeginsTestCases :: [NofUniqueBeginsTestCase]
buildNofUniqueBeginsTestCases =
  [ f "empty input"
      (makeFollowupStartedByIndex 10, concur, isEnrollmentEvent)
      (0, 1)
      []
      []
      {-
         -                    <- Index
         ----------           <- Baseline

        |--------------|
      -}
  , f "2 results if 2 different begins"
      (makeFollowupStartedByIndex 10, concur, containsConcepts ["A"])
      (0, 1)
      [h ["A"] (2, 5), h ["A"] (4, 5)]
      [(2, 1), (4, 2)]
      {-
         -                    <- Index
         ----------           <- Followup
           ---                <- "A"
             _                <- "A"
        |--------------|
      -}
  , f "2 results when multiple begins at same time"
      (makeFollowupStartedByIndex 10, concur, containsConcepts ["A"])
      (0, 1)
      [h ["A"] (2, 3), h ["A"] (2, 5), h ["A"] (4, 5)]
      [(2, 1), (4, 2)]
      {-
         -                    <- Index
         ----------           <- Followup 
           -                  <- "A"
           ---                <- "A"
             -                <- "A"
        |--------------|
      -}
  , f "1 result based on predicate filter"
      (makeFollowupStartedByIndex 10, concur, containsConcepts ["A"])
      (0, 1)
      [h ["B"] (2, 3), h ["B"] (2, 5), h ["A"] (4, 5)]
      [(4, 1)]
      {-
         -                    <- Index
         ----------           <- Followup 
           -                  <- "B"
           ---                <- "B"
             -                <- "A"
        |--------------|
      -}
  ] where
  f = makeTestCaseOfIndexAndEvents
  h = makeEventWithConcepts

buildNofUniqueBeginsTests :: TestTree
buildNofUniqueBeginsTests = makeTestGroup "Tests ofNofUniqueBegins template"
                                          buildNofUniqueBegins
                                          buildNofUniqueBeginsTestCases

