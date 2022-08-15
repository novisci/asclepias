{-# LANGUAGE DataKinds #-}
module FeatureExamples.DrugDiscontinuation
  ( example
  ) where


import           CohortExamples.CreateAssessmentInterval (flwup)
import           ExampleEvents
import           Hasklepias
-- | time of distcontinuation of antibiotics
--   and time from start of follow up

{- tag::function[] -}
discontinuation
  :: (IntervalSizeable a b)
  => AssessmentInterval a
  -> [Event Text ExampleModel a]
  -> Maybe (a, b)
discontinuation i events =
  events
    |> filterEvents (containsTag ["tookAntibiotics"]) -- <1>
    |> fmap (expandr 5) -- <2>
    |> combineIntervals -- <3>
    |> nothingIfNone (startedBy <|> overlappedBy $ i) -- <4>
    |> (>>= gapsWithin i) -- <5>
    |> (>>= headMay) -- <6>
    |> fmap (\x -> (begin x, diff (begin x) (begin i))) -- <7>
{- end::function[] -}

{- tag::definition[] -}
discontinuationDef
  :: (IntervalSizeable a b)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" [Event Text ExampleModel a]
       -> Feature "discontinuation" (Maybe (a, b))
       )
discontinuationDef = define discontinuation
{- end::definition[] -}

ev i t = event i (context (packTagSet [t]) Medical Nothing)

exampleFollowup = makeFollowupStartedByIndex 10 (beginervalMoment 5)

case1 :: [ExampleEvent]
case1 =
  [ ev (beginerval 3 1)  "tookAntibiotics"
  , ev (beginerval 2 4)  "tookAntibiotics"
  , ev (beginerval 2 8)  "wasHospitalized"
  , ev (beginerval 5 10) "tookAntibiotics"
  ]

case2 :: [ExampleEvent]
case2 =
  [ ev (beginerval 2 6)  "tookAntibiotics"
  , ev (beginerval 5 10) "tookAntibiotics"
  ]

case3 :: [ExampleEvent]
case3 = [ev (beginerval 3 5) "tookAntibiotics"]

example :: TestTree
example = testGroup
  "Tests of discontinuation"
  [ testCase "using exampleEvents1"
  $   discontinuation (flwup (beginervalMoment 60)) exampleEvents1
  @?= Just (78, 18)
  , testCase "Case 1" $ discontinuation exampleFollowup case1 @?= Nothing
  , testCase "Case 2" $ discontinuation exampleFollowup case2 @?= Nothing
  , testCase "Case 3" $ discontinuation exampleFollowup case3 @?= Just (13, 8)
  ]
