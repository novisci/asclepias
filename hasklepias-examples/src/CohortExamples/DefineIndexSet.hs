module CohortExamples.DefineIndexSet
  ( example
  ) where

import           ExampleEvents
import           Hasklepias
{-
Index is defined as the first occurrence of an Orca bite.
-}
{- tag::defineindex[] -}
defineIndexSet
  :: Ord a
  =>  -- <1>
     [Event Text ExampleModel a]
  -> -- <2>
     IndexSet (Interval a) -- <3>
defineIndexSet events =
  events
    |> filterEvents (containsConcepts ["wasBitByOrca"]) -- <4>
    |> headMay -- <5>
    |> fmap getInterval -- <6>
    |> into -- <7>
{- end::defineindex[] -}

example :: TestTree
example = testGroup
  "define indexset example"
  [ testCase "" $ defineIndexSet exampleEvents1 @?= makeIndexSet
      [beginervalMoment 60]
  ]
