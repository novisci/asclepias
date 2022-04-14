{-|
Description : Demostrates how to define features using Hasklepias
-}

module FeatureExamples.Example3
  ( example
  ) where

import           ExampleEvents                  ( exampleEvents4 )
import           Hasklepias

{- tag::function[] -}
examplePairComparison
  :: (Eventable c m a, IntervalSizeable a b)
  => ([c], [c])
  -> Interval a
  -> [Event c m a]
  -> Maybe a
examplePairComparison (c1, c2) i =
  filterConcur i    --  <1>
    .> splitByConcepts c1 c2 -- <2>
    .> uncurry allPairs -- <3>
    .> filter (\pr -> fst pr `concur` expand 3 3 (snd pr)) -- <4>
    .> lastMay -- <5>
    .> fmap (begin . fst) -- <6>
{- end::function[] -}

{- tag::definition[] -}
def
  :: (Eventable c m a, IntervalSizeable a b)
  => ([c], [c])
  -> Def (F n1 (Interval a) -> F n2 [Event c m a] -> F n3 (Maybe a))
def cpts = define (examplePairComparison cpts)
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
