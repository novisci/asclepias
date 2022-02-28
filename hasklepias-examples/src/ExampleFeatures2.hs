{-|
Module      : ExampleFeatures2
Description : Demostrates how to define features using Hasklepias
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module ExampleFeatures2
  ( durationOfHospitalizedAntibiotics
  ) where

import           Hasklepias

type ConceptEvent a = PairedInterval (Concepts Text) a

toConceptEvent :: (Ord a) => Event ClaimsSchema Text a -> ConceptEvent a
toConceptEvent x =
  makePairedInterval (getConcepts $ getContext x) (getInterval x)

durationOfHospitalizedAntibiotics
  :: (Show a, IntervalSizeable a b, Ord a)
  => [Event ClaimsSchema Text a]
  -> FeatureData [b]
durationOfHospitalizedAntibiotics =
  filter (`hasAnyConcepts` cpts) .> fmap toConceptEvent .> \x -> if null x
    then featureDataL $ Other "no cases"
    else
      x
      |> formMeetingSequence
      |> filter (\z -> hasAllConcepts (getPairData z) cpts)
      |> durations
      |> pure
  where cpts = ["tookAntibiotics", "wasHospitalized"] :: [Text]
