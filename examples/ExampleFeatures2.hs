{-|
Module      : ExampleFeatures2
Description : Demostrates how to define features using Hasklepias
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module ExampleFeatures2(
    exampleFeatures2Spec
) where

import Hasklepias
import ExampleEvents
import Test.Hspec

durationOfHospitalizedAntibiotics:: (Intervallic (PairedInterval Concepts) a
                                    , Show a
                                    , Intervallic Interval a
                                    , IntervalSizeable a b) =>
     Events a
  -> FeatureData [b]
durationOfHospitalizedAntibiotics es
    | null y    = featureL $ Other "no cases"
    | otherwise = featureR $ durations y
    where conceptsText = ["wasHospitalized", "tookAntibiotics"] 
          concepts = map packConcept conceptsText
          x = formMeetingSequence (map (toConceptEventOf concepts) es)
          y = filter (\z -> hasAllConcepts (getPairData z) conceptsText ) x 


exampleFeatures2Spec :: Spec
exampleFeatures2Spec = do

    it "durationOfHospitalizedAntibiotics from exampleEvents1" $
        durationOfHospitalizedAntibiotics exampleEvents1 `shouldBe` 
            featureL (Other "no cases")

    it "durationOfHospitalizedAntibiotics from exampleEvents3" $
        durationOfHospitalizedAntibiotics exampleEvents3 `shouldBe` 
            featureR [3, 2]