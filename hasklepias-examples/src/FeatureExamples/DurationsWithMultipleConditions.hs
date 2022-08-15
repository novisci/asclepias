{-|
Description : Demostrates how to define features using Hasklepias
-}

module FeatureExamples.DurationsWithMultipleConditions
  ( example
  ) where

import           ExampleEvents (exampleEvents4)
import           Hasklepias

{- tag::function[] -}
examplePairComparison
  :: (Eventable t m a, IntervalSizeable a b)
  => ([t], [t])
  -> Interval a
  -> [Event t m a]
  -> Maybe a
examplePairComparison (t1, t2) i =
  filterConcur i    --  <1>
    .> splitByTags t1 t2 -- <2>
    .> uncurry allPairs -- <3>
    .> filter (\pr -> fst pr `concur` expand 3 3 (snd pr)) -- <4>
    .> lastMay -- <5>
    .> fmap (begin . fst) -- <6>
{- end::function[] -}

{- tag::definition[] -}
def
  :: (Eventable t m a, IntervalSizeable a b)
  => ([t], [t])
  -> Def (F n1 (Interval a) -> F n2 [Event t m a] -> F n3 (Maybe a))
def tag = define (examplePairComparison tag)
{- end::definition[] -}

flwup :: (Interval Int)
flwup = beginerval 50 0

example :: TestTree
example = testGroup
  "examplePairComparison"
  [ testCase ""
    $   examplePairComparison (["c1"], ["c2"]) flwup exampleEvents4
    @?= Just 16
  ]
