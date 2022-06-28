{-# LANGUAGE DataKinds #-}
module FeatureExamples.CountOfHospitalEvents
  ( example
  ) where
import           ExampleEvents
import           Hasklepias

-- | Count of hospital events in a interval and duration of the last one
{- tag::function[] -}
countOfHospitalEvents
  :: (IntervalSizeable a b)
  => AssessmentInterval a
  -> [Event Text ExampleModel a]
  -> (Int, Maybe b)
countOfHospitalEvents i =
  filterEvents (containsTag ["wasHospitalized"]) -- <1>
    .> combineIntervals -- <2>
    .> filterConcur i -- <3>
    .> (\x -> (length x, duration <$> lastMay x)) -- <4>
{- end::function[] -}

{- tag::definition[] -}
countOfHospitalEventsDef
  :: (IntervalSizeable a b)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" [Event Text ExampleModel a]
       -> Feature "count of hospitalizations" (Int, Maybe b)
       )
countOfHospitalEventsDef = define countOfHospitalEvents
{- end::definition[] -}

ev i c = event i (context (packTagSet [c]) Medical Nothing)

exampleFollowup = makeFollowupStartedByIndex 10 (beginervalMoment 5)

case1 :: [ExampleEvent]
case1 =
  [ ev (beginerval 1 5)  "wasHospitalized"
  , ev (beginerval 4 2)  "wasHospitalized"
  , ev (beginerval 2 8)  "notHospitalized"
  , ev (beginerval 5 10) "wasHospitalized"
  ]


example :: TestTree
example = testGroup
  "Tests of countOfHospitalEvents"
  [ testCase "using exampleEvents1"
  $   countOfHospitalEvents
        (makeFollowupStartedByIndex 20 (beginervalMoment 50))
        exampleEvents1
  @?= (1, Just 8)
  , testCase "Case 1"
  $   countOfHospitalEvents exampleFollowup case1
  @?= (2, Just 5)
  ]
