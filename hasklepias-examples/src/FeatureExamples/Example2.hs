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

{- tag::code[] -}
durationsOf
  :: (KnownSymbol n, Eventable d c a, IntervalSizeable a b)
  => [c]
  -> [Event d c a]
  -> Feature n [b]
durationsOf cpts =
  filter (`hasAnyConcepts` cpts) -- <1>
    .> fmap (first getConcepts . getEvent) -- <2> <3>
    .> formMeetingSequence -- <4>
    .> filter (\z -> hasAllConcepts (getPairData z) cpts) -- <5>
    .> \x -> if null x -- <6>
         then makeFeature $ featureDataL $ Other "no cases"
         else makeFeature $ featureDataR (durations x)
{- end::code[] -}

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
