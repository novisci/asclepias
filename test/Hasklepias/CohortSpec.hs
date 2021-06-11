{-# LANGUAGE OverloadedStrings #-}

module Hasklepias.CohortSpec (
  spec
 ) where

import FeatureCompose
import Hasklepias.Cohort
import Test.Hspec ( describe, pending, shouldBe, it, Spec )
import Data.Map.Strict

d1 :: FeatureSpec () (FeatureData Int) Bool
d1 =
  specifyFeature
    "feat1" ()
    (defineM
    (\x ->
      if x < 0 then
        featureDataL $ Other "at least 1 < 0"
      else
        pure ((x + 1) == 5)
    ))

d2 :: FeatureSpec () (FeatureData Int) Status
d2 = specifyFeature "feat2" () (define (\x -> includeIf (x*2 > 4)))

d3 ::  FeatureSpec () (FeatureData Int) Int
d3 = specifyFeature "feat3" () (define (+ 2))

testSubject1 :: Subject Int
testSubject1 = MkSubject ("1", 0)
testSubject2 :: Subject Int
testSubject2 = MkSubject ("2", 54)

testPopulation :: Population Int
testPopulation = MkPopulation [testSubject1, testSubject2]

buildCriteria :: Int -> Criteria ()
buildCriteria dat = criteria $ pure ( criterion feat1 )
  where feat1 = evalSpec d2 $ pure dat

type Features = (Feature () Bool, Feature () Int)
buildFeatures :: Int -> Features
buildFeatures dat  =
    ( evalSpec d1 input
    , evalSpec d3 input
    )
    where input = pure dat

testCohort :: CohortSpec () Int Features
testCohort = specifyCohort buildCriteria buildFeatures

testOut :: Cohort Features
testOut = MkCohort
  ( MkAttritionInfo [ (ExcludedBy (1, "feat2"), 1), (Included, 1)]
  , [MkObsUnit ("2", ( MkFeature "feat1" () (featureDataR False)
                     , MkFeature "feat3" () (featureDataR 56))) ])

-- evalCohort testCohort testPopulation
spec :: Spec
spec = do

  describe "checking d1" $
    do

      it "include f1" $
        evalCohort testCohort testPopulation `shouldBe` testOut

