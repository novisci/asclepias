{-|
Description : Demostrates how to define features using Hasklepias
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module FeatureExamples.Example2
  ( example
  ) where

import           ExampleEvents
import           Hasklepias

{- tag::function[] -}
durationsOf
  :: (KnownSymbol n, Eventable d c a, IntervalSizeable a b)
  => [c]
  -> [Event d c a]
  -> Feature n [b]
durationsOf cpts =
  filter (`hasAnyConcepts` cpts) -- <1>
    .> fmap (first getConcepts . getEvent) -- <2> <3>
    .> formMeetingSequence -- <4>
    .> filter (`hasAllConcepts` cpts) -- <5>
    .> \x -> if null x -- <6>
         then makeFeature $ featureDataL $ Other "no cases"
         else makeFeature $ featureDataR (durations x)
{- end::function[] -}

{- tag::definition[] -}
def
  :: (KnownSymbol n1, KnownSymbol n2, Eventable d c a, IntervalSizeable a b)
  => [c] -- <1>
  -> Def (F n1 [Event d c a] -> F n2 [b]) -- <2>
def cpts = defineA (durationsOf cpts)
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
