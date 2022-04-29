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

{- tag::function[] -}
makeHx
  :: (Ord a)
  => [Text] -- <1>
  -> AssessmentInterval a
  -> [Event Text ExampleModel a]
  -> Maybe (Interval a) -- <2>
makeHx cpts i events =
  events
    |> filterEvents (containsConcepts cpts &&& Predicate (enclose i)) -- <3>
    |> lastMay -- <4> 
    |> fmap getInterval -- <5>
{- end::function[] -}

{- tag::definition[] -}
duckHxDef -- <1>
  :: (Ord a)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" [Event Text ExampleModel a]
       -> Feature "duck history" (Maybe (Interval a))
       )
duckHxDef = define (makeHx ["wasBitByDuck", "wasStruckByDuck"])

macawHxDef -- <2>
  :: (Ord a)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" [Event Text ExampleModel a]
       -> Feature "macaw history" (Maybe (Interval a))
       )
macawHxDef = define (makeHx ["wasBitByMacaw", "wasStruckByMacaw"])
{- end::definition[] -}

example :: TestTree
example = testGroup
  "History of event tests"
  [ testCase "exampleEvents4"
    $   makeHx ["c1"] (flwup (beginervalMoment 0)) exampleEvents4
    @?= Just (beginerval 9 16)
  ]
