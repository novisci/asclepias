{-|
Description : Demostrates how to define features using Hasklepias
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FeatureExamples.Example2
  ( example
  ) where

import           ExampleEvents
import           Hasklepias

{- tag::function[] -}
durationsOf
  :: forall n m c a b
   . (KnownSymbol n, Eventable c m a, IntervalSizeable a b)
  => [c]
  -> [Event c m a]
  -> Feature n [b]
durationsOf cpts =
  filter (`hasAnyConcepts` cpts) -- <1>
    .> fmap (into @(ConceptsInterval c a)) -- <2> <3>
    .> formMeetingSequence -- <4>
    .> filter (`hasAllConcepts` cpts) -- <5>
    .> \x -> if null x -- <6>
         then makeFeature $ featureDataL $ Other "no cases"
         else makeFeature $ featureDataR (durations x)
{- end::function[] -}

{- tag::definition[] -}
def
  :: (KnownSymbol n1, KnownSymbol n2, Eventable c m a, IntervalSizeable a b)
  => [c] -- <1>
  -> Def (F n1 [Event c m a] -> F n2 [b]) -- <2>
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
