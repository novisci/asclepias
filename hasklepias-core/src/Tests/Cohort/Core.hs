{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Tests.Cohort.Core
  ( tests
  ) where

import           Cohort
import           Cohort.Attrition
import           Data.Set                       ( empty
                                                , fromList
                                                , singleton
                                                )
import           Data.Text
import           Features
import           IntervalAlgebra
import           Test.Tasty
import           Test.Tasty.HUnit
import           Witch

d1 :: Definition (Feature "feat" Int -> Feature "feat1" Bool)
d1 = defineA
  (\x -> if x < 0
    then makeFeature $ featureDataL $ Other "at least 1 < 0"
    else pure ((x + 1) == 5)
  )


d2 :: Definition (Feature "feat" Int -> Feature "feat2" Status)
d2 = define (\x -> includeIf (x * 2 > 4))

d3 :: Definition (Feature "feat" Int -> Feature "feat3" Int)
d3 = define (+ 2)


testSubject1 :: Subject Int
testSubject1 = into ("1" :: Text, 0 :: Int)
testSubject2 :: Subject Int
testSubject2 = into ("2" :: Text, 54 :: Int)

testPopulation :: Population Int
testPopulation = into [testSubject1, testSubject2]

-- create a dummy index
buildIndices :: Int -> IndexSet (Interval Int)
buildIndices i = makeIndexSet [beginerval 1 i]

buildCriteria :: Interval Int -> Int -> Criteria
buildCriteria _ dat = criteria $ pure (criterion feat1)
  where feat1 = eval d2 $ pure dat

type Features = (Feature "feat1" Bool, Feature "feat3" Int)
buildFeatures :: Interval Int -> Int -> Features
buildFeatures _ dat = (eval d1 input, eval d3 input) where input = pure dat

testCohort :: CohortSpec Int Features (Interval Int)
testCohort = specifyCohort buildIndices buildCriteria buildFeatures

testFeatures :: (F "feat1" Bool, F "feat3" Int)
testFeatures =
  (makeFeature (featureDataR False), makeFeature (featureDataR 56))

anID :: ObsID (Interval Int)
anID = makeObsID (beginerval (1 :: Int) 54) ("2" :: Text)

aUnit :: ObsUnit Features (Interval Int)
aUnit = from @(ObsID (Interval Int), Features) (anID, testFeatures)


testOut :: Cohort Features (Interval Int)
testOut = MkCohort
  ( Just $ MkAttritionInfo 2 2 $ fromList
    [ MkAttritionLevel SubjectHasNoIndex         0
    , MkAttritionLevel (ExcludedBy (1, "feat2")) 1
    , MkAttritionLevel Included                  1
    ]
  , into @(CohortData Features (Interval Int)) [aUnit]
  )

tests :: TestTree
tests = testGroup
  "Unit tests on Cohort.Core"
  [ testCase "testCohort evaluates to testOut"
    $   makeCohortEvaluator defaultCohortEvalOptions testCohort testPopulation
    --    evalCohort testCohort testPopulation
    @?= pure @[] testOut
  ]
