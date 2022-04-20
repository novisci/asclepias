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
  => [Event Text ExampleModel a] -- <1>
  -> IndexSet (Interval a) -- <2>
defineIndexSet events =
  events
    |> filterEvents (containsConcepts ["wasBitByOrca"]) -- <3>
    |> headMay -- <4>
    |> fmap getInterval -- <5>
    |> into -- <6>
{- end::defineindex[] -}

example :: TestTree
example = testGroup
  "define indexset example"
  [ testCase "" $ defineIndexSet exampleEvents1 @?= makeIndexSet
      [beginervalMoment 60]
  ]
