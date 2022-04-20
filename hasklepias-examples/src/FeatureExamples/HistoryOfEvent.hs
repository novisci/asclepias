{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FeatureExamples.HistoryOfEvent where

import           CohortExamples.CreateAssessmentInterval
                                                ( flwup )
import           ExampleEvents
import           Hasklepias


{-
Define features that identify whether a subject was bit/struck by a duck and
bit/struck by a macaw.
-}
{- tag::exampleFunction[] -}
makeHx
  :: (Ord a)
  => [Text]
  -> AssessmentInterval a
  -> [Event Text ExampleModel a]
  -> Maybe (Interval a)
makeHx cpts i events =
  events
    |> filterEvents (containsConcepts cpts &&& Predicate (enclose i))
    |> lastMay
    |> fmap getInterval
{- end::exampleFunction[] -}

{- tag::exampleDefinition1 -}
duckHxDef
  :: (Ord a)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" [Event Text ExampleModel a]
       -> Feature "duck history" (Maybe (Interval a))
       )
duckHxDef = define (makeHx ["wasBitByDuck", "wasStruckByDuck"])
{- end::exampleDefinition1 -}

{- tag::exampleDefinition2-}
macawHxDef
  :: (Ord a)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" [Event Text ExampleModel a]
       -> Feature "macaw history" (Maybe (Interval a))
       )
macawHxDef = define (makeHx ["wasBitByMacaw", "wasStruckByMacaw"])
{- end::exampleDefinition2 -}

example :: TestTree
example = testGroup
  "makeHx on exampleEvents4"
  [ testCase ""
    $   makeHx ["c1"] (flwup (beginervalMoment 0)) exampleEvents4
    @?= Just (beginerval 9 16)
  ]
