{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Tests.Cohort.Criteria
  ( tests
  ) where

import           Cohort.Criteria
import           Test.Tasty
import           Test.Tasty.HUnit
import           Witch

f1 :: Status -> Criterion
f1 = makeCriterion "f1"

f2 :: Status -> Criterion
f2 = makeCriterion "f2"

f3 :: Status -> Criterion
f3 = makeCriterion "f3"

f4 :: CriterionThatCanFail
f4 = Left $ MkCriterionFailure "bad"

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
  $   checkCohortStatus (makeCriteriaPure $ pure (f1 Include))
  @?= Included
  , testCase "include f1, f2, f3"
  $ checkCohortStatus (makeCriteriaPure $ f1 Include : [f2 Include, f3 Include])
  @?= Included
  , testCase "exclude on f2"
  $   checkCohortStatus (makeCriteriaPure $ f2 Exclude : [f3 Include])
  @?= ExcludedBy (1, "f2")
  , testCase "exclude on f2"
  $ checkCohortStatus (makeCriteriaPure $ f1 Include : [f2 Exclude, f3 Include])
  @?= ExcludedBy (2, "f2")
  , testCase "error on f4"
  $   checkCohortStatus
        ( makeCriteria
        $ pure (f1 Include)
        : [pure $ f2 Include, pure $ f3 Include, f4]
        )
  @?= CriteriaFailure "bad"
  , testCase "measuring attrition that includes a failure"
  $   measureSubjectAttrition
        (Just
          ( makeCriteria
          $ pure (f1 Include)
          : [pure $ f2 Include, pure $ f3 Include, f4]
          )
        )
        [ checkCohortStatus
          ( makeCriteria
          $ pure (f1 Include)
          : [pure $ f2 Include, pure $ f3 Include, f4]
          )
        , checkCohortStatus
          ( makeCriteria
          $ pure (f1 Include)
          : [pure $ f2 Include, pure $ f3 Include]
          )
        ]
  @?= makeTestAttritionInfo 1 2 [(CriteriaFailure "bad", 1), (Included, 1)]
  , testCase "semigroup: testAttr1 <> testAttr2"
  $   testAttr1
  <>  testAttr2
  @?= testAttr1p2
  ]



