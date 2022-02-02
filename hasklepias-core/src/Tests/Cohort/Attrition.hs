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
testAttr1 = MkAttritionInfo 2 $ fromList
  [ MkAttritionLevel SubjectHasNoIndex         0
  , MkAttritionLevel (ExcludedBy (1, "feat2")) 1
  , MkAttritionLevel Included                  1
  ]

testAttr2 :: AttritionInfo
testAttr2 = MkAttritionInfo 5 $ fromList
  [ MkAttritionLevel SubjectHasNoIndex         0
  , MkAttritionLevel (ExcludedBy (1, "feat2")) 3
  , MkAttritionLevel Included                  2
  ]

testAttr1p2 :: AttritionInfo
testAttr1p2 = MkAttritionInfo 7 $ fromList
  [ MkAttritionLevel SubjectHasNoIndex         0
  , MkAttritionLevel (ExcludedBy (1, "feat2")) 4
  , MkAttritionLevel Included                  3
  ]

tests :: TestTree
tests = testGroup
  "Unit tests on Cohort.Attrition"
  [ testCase "semigroup: testAttr1 <> testAttr2"
    $   testAttr1
    <>  testAttr2
    @?= testAttr1p2
  ]
