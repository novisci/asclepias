{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Tests.Cohort.Criteria
  ( tests
  ) where

import           Cohort.Criteria
import           Data.List.NonEmpty
import           IntervalAlgebra
import           Test.Tasty
import           Test.Tasty.HUnit
import           Witch

f1 :: Status -> Criterion
f1 = criterion "f1"
-- f1 s = int (makeFeature (featureDataR s) :: Feature "f1" Status)

f2 :: Status -> Criterion
f2 = criterion "f2"
-- f2 s = into @Criterion (makeFeature (featureDataR s) :: Feature "f2" Status)

f3 :: Status -> Criterion
f3 = criterion "f3"
-- f3 s = into @Criterion (makeFeature (featureDataR s) :: Feature "f3" Status)

f4 :: Criterion
f4 = criterion "f4" Exclude
-- f4 = into @Criterion 
--   (makeFeature (featureDataL $ Other "something") :: Feature "f4" Status)

index :: Maybe (Interval Int)
index = Just (beginerval 1 1)

testAttr1 :: AttritionInfo
testAttr1 = makeTestAttritionInfo
  1
  2
  [(SubjectHasNoIndex, 0), (ExcludedBy (1, "feat2"), 1), (Included, 1)]

testAttr2 :: AttritionInfo
testAttr2 = makeTestAttritionInfo
  1
  5
  [(SubjectHasNoIndex, 0), (ExcludedBy (1, "feat2"), 3), (Included, 2)]

testAttr1p2 :: AttritionInfo
testAttr1p2 = makeTestAttritionInfo
  2
  7
  [(SubjectHasNoIndex, 0), (ExcludedBy (1, "feat2"), 4), (Included, 3)]

tests :: TestTree
tests = testGroup
  "Unit tests on Cohort.Criteria"
  [ testCase "include f1"
  $   checkCohortStatus index (criteria $ pure (f1 Include))
  @?= Included
  , testCase "include f1, f2, f3"
  $   checkCohortStatus index (criteria $ f1 Include : [f2 Include, f3 Include])
  @?= Included
  , testCase "exclude on f2"
  $   checkCohortStatus index (criteria $ f2 Exclude : [f3 Include])
  @?= ExcludedBy (1, "f2")
  , testCase "exclude on f2"
  $   checkCohortStatus index (criteria $ f1 Include : [f2 Exclude, f3 Include])
  @?= ExcludedBy (2, "f2")
  , testCase "error on f4"
  $   checkCohortStatus index
                        (criteria $ f1 Include : [f2 Include, f3 Include, f4])
  @?= ExcludedBy (4, "f4")
  , testCase "semigroup: testAttr1 <> testAttr2"
  $   testAttr1
  <>  testAttr2
  @?= testAttr1p2
  ]



