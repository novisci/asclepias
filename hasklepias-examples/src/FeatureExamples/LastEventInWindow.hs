{-|
Description : Demostrates how to define features using Hasklepias
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module FeatureExamples.LastEventInWindow
  ( example
  ) where

import           ExampleEvents
import           Hasklepias

{- tag::function[] -}
durationsOf
  :: forall n m t a b
   . (KnownSymbol n, Eventable t m a, IntervalSizeable a b)
  => [t]
  -> [Event t m a]
  -> Feature n [b]
durationsOf tSet =
  filter (`hasAnyTag` tSet) -- <1>
    .> fmap (into @(TagSetInterval t a)) -- <2> <3>
    .> formMeetingSequence -- <4>
    .> filter (`hasAllTags` tSet) -- <5>
    .> \x -> if null x -- <6>
         then makeFeature $ featureDataL $ Other "no cases"
         else makeFeature $ featureDataR (durations x)
{- end::function[] -}

{- tag::definition[] -}
def
  :: (KnownSymbol n1, KnownSymbol n2, Eventable t m a, IntervalSizeable a b)
  => [t] -- <1>
  -> Def (F n1 [Event t m a] -> F n2 [b]) -- <2>
def tSet = defineA (durationsOf tSet)
{- end::definition[] -}

example :: TestTree
example = testGroup
  "durationsOf example"
  [ testCase "test on exampleEvents1"
  $   durationsOf (["tookAntibiotics", "wasHospitalized"] :: [Text])
                  exampleEvents1
  @?= makeFeature @"foo" (featureDataL (Other "no cases"))
  , testCase "test on exampleEvents3"
  $   durationsOf (["tookAntibiotics", "wasHospitalized"] :: [Text])
                  exampleEvents3
  @?= makeFeature @"foo" (featureDataR [3, 2])
  ]
