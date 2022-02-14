{-# LANGUAGE OverloadedStrings #-}

module Tests.Cohort.Attrition
  ( tests
  ) where

import           Cohort.Attrition
import           Cohort.Criteria
import           Data.Set
import           Test.Tasty
import           Test.Tasty.HUnit

testAttr1 :: AttritionInfo
testAttr1 = makeTestAttritionInfo 1 2 
  [ (SubjectHasNoIndex, 0)
  , (ExcludedBy (1, "feat2"), 1)
  , (Included, 1)]

testAttr2 :: AttritionInfo
testAttr2 = makeTestAttritionInfo 1 5 
  [ (SubjectHasNoIndex, 0)
  , (ExcludedBy (1, "feat2"), 3)
  , (Included, 2)]
  
testAttr1p2 :: AttritionInfo
testAttr1p2 = makeTestAttritionInfo 2 7 
  [ (SubjectHasNoIndex, 0)
  , (ExcludedBy (1, "feat2"), 4)
  , (Included, 3)]

tests :: TestTree
tests = testGroup
  "Unit tests on Cohort.Attrition"
  [ testCase "semigroup: testAttr1 <> testAttr2"
    $   testAttr1
    <>  testAttr2
    @?= testAttr1p2
  ]
