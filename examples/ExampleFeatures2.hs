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
  ( exampleFeatures2Spec
  ) where

import           ExampleEvents
import           Hasklepias
import           Test.Hspec

durationOfHospitalizedAntibiotics
  :: (Show a, IntervalSizeable a b) => Events a -> FeatureData [b]
durationOfHospitalizedAntibiotics es | null y = featureDataL $ Other "no cases"
                                     | otherwise = pure $ durations y
 where
  conceptsText = ["wasHospitalized", "tookAntibiotics"]
  concepts     = map packConcept conceptsText
  x            = formMeetingSequence (map (toConceptEventOf concepts) es)
  y            = filter (\z -> hasAllConcepts (getPairData z) conceptsText) x


exampleFeatures2Spec :: Spec
exampleFeatures2Spec = do

  it "durationOfHospitalizedAntibiotics from exampleEvents1"
    $          durationOfHospitalizedAntibiotics exampleEvents1
    `shouldBe` featureDataL (Other "no cases")

  it "durationOfHospitalizedAntibiotics from exampleEvents3"
    $          durationOfHospitalizedAntibiotics exampleEvents3
    `shouldBe` pure [3, 2]
