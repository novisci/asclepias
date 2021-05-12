{-|
Module      : ExampleFeatures1
Description : Demostrates how to define features using Hasklepias
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE MonoLocalBinds #-}
module ExampleFeatures2(
    exampleFeatures2Spec
) where

import Hasklepias
import ExampleEvents
import Test.Hspec
import Data.Text(pack, Text)
import Data.Maybe ( fromMaybe )
import Data.Set(member)

hasAllConcepts :: [Concept] -> Concepts -> Bool
hasAllConcepts c s = all (`member` s) c

durationOfHospitalizedAntibiotics:: (IntervalAlgebraic (PairedInterval Concepts) a
                                    , IntervalAlgebraic Interval a
                                    , IntervalSizeable a b) =>
     Events a
  -> Feature [b]
durationOfHospitalizedAntibiotics es
    | null y    = Feature $ Left $ Other "no cases"
    | otherwise = Feature $ Right $ durations y
    where concepts = map packConcept ["wasHospitalized", "tookAntibiotics"]
          x = formMeetingSequence (map (toConceptEventOf concepts) es)
          y = filter (hasAllConcepts concepts . getPairData) x 


exampleFeatures2Spec :: Spec
exampleFeatures2Spec = do

    it "durationOfHospitalizedAntibiotics from exampleEvents1" $
        durationOfHospitalizedAntibiotics exampleEvents1 `shouldBe` 
            Feature (Left (Other "no cases"))

    it "durationOfHospitalizedAntibiotics from exampleEvents3" $
        durationOfHospitalizedAntibiotics exampleEvents3 `shouldBe` 
            Feature (Right [3, 2])