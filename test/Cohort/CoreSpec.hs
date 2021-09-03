{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Cohort.CoreSpec (
  spec
 ) where

import Features
import Cohort
import Data.Set (fromList)
import Test.Hspec ( describe, pending, shouldBe, it, Spec )

-- data Feat1

d1 :: Definition (Feature "feat" Int -> Feature "feat1" Bool)
d1 =
    defineA
    (\x ->
      if x < 0 then
        makeFeature $ featureDataL $ Other "at least 1 < 0"
      else
        pure ((x + 1) == 5)
    )


d2 :: Definition (Feature "feat" Int -> Feature "feat2" Status)
d2 = define (\x -> includeIf (x*2 > 4))

d3 :: Definition (Feature "feat" Int -> Feature  "feat3" Int)
d3 = define (+ 2)


testSubject1 :: Subject Int
testSubject1 = MkSubject ("1", 0)
testSubject2 :: Subject Int
testSubject2 = MkSubject ("2", 54)

testPopulation :: Population Int
testPopulation = MkPopulation [testSubject1, testSubject2]

buildCriteria :: Int -> Criteria
buildCriteria dat = criteria $ pure ( criterion feat1 )
  where feat1 = eval d2 $ pure dat

type Features = (Feature "feat1" Bool, Feature "feat3" Int)
buildFeatures :: Int -> Features
buildFeatures dat  =
    ( eval d1 input
    , eval d3 input
    )
    where input = pure dat

testCohort :: CohortSpec Int Features
testCohort = specifyCohort buildCriteria buildFeatures

testOut :: Cohort Features
testOut = MkCohort
  ( MkAttritionInfo 2 $
   fromList [ uncurry MkAttritionLevel (ExcludedBy (1, "feat2"), 1)
            , uncurry MkAttritionLevel (Included, 1) ]
  , MkCohortData [MkObsUnit "2" 
                  ( makeFeature (featureDataR False)
                  , makeFeature (featureDataR 56)) ])

testAttr1 :: AttritionInfo
testAttr1 =  MkAttritionInfo 2 $
   fromList [ uncurry MkAttritionLevel (ExcludedBy (1, "feat2"), 1)
            , uncurry MkAttritionLevel (Included, 1) ] 

testAttr2 :: AttritionInfo
testAttr2 =  MkAttritionInfo 5 $
   fromList [ uncurry MkAttritionLevel (ExcludedBy (1, "feat2"), 3)
            , uncurry MkAttritionLevel (Included, 2) ] 

testAttr1p2 :: AttritionInfo
testAttr1p2 =  MkAttritionInfo 7 $
   fromList [ uncurry MkAttritionLevel (ExcludedBy (1, "feat2"), 4)
            , uncurry MkAttritionLevel (Included, 3) ]

-- evalCohort testCohort testPopulation
spec :: Spec
spec = do

  describe "checking evaluation of a cohort" $
    do
      it "testCohort evaluates to testOut" $
        evalCohort testCohort testPopulation `shouldBe` testOut

  describe "checking semigroup of attrition" $
    do
      it "testAttr1 <> testAttr2" $
        testAttr1 <> testAttr2 `shouldBe` testAttr1p2