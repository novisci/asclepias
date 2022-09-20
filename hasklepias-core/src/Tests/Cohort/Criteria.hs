{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Tests.Cohort.Criteria
  ( tests
  ) where

import           Cohort.Criteria
import           Data.Map.Strict  as Map (toList)
import           GHC.Natural
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
f4 = Left "bad"

type ExpectedAttrition = (Int, Int, [(CohortStatus, Natural)])

toExpectedAttrition :: AttritionInfo -> ExpectedAttrition
toExpectedAttrition x =
  ( totalSubjectsProcessed x
  , totalUnitsProcessed x
  , toList $ attritionInfo x)

testAttr1 :: ExpectedAttrition
testAttr1 =
  ( 1
  , 2
  , [(SubjectHasNoIndex, 0), (ExcludedBy (1, "feat2"), 1), (Included, 1)]
  )

testAttr2 :: ExpectedAttrition
testAttr2 =
  ( 1
  , 5
  , [(SubjectHasNoIndex, 0), (ExcludedBy (1, "feat2"), 3), (Included, 2)]
  )

testAttr1p2 :: ExpectedAttrition
testAttr1p2 =
  ( 2
  , 7
  , [(SubjectHasNoIndex, 0), (ExcludedBy (1, "feat2"), 4), (Included, 3)]
  )

tests :: TestTree
tests = testGroup
  "Unit tests on Cohort.Criteria"
  [ testCase "include f1"
  $   checkCohortStatus (into @Criteria $ [f1 Include])
  @?= Included
  , testCase "include f1, f2, f3"
  $ checkCohortStatus (into @Criteria $ f1 Include : [f2 Include, f3 Include])
  @?= Included
  , testCase "exclude on f2"
  $   checkCohortStatus (into @Criteria $ f2 Exclude : [f3 Include])
  @?= ExcludedBy (1, "f2")
  , testCase "exclude on f2"
  $ checkCohortStatus (into @Criteria $ f1 Include : [f2 Exclude, f3 Include])
  @?= ExcludedBy (2, "f2")
  , testCase "error on f4"
  $   checkCohortStatus
        ( into @Criteria
        $ pure (f1 Include)
        : [pure $ f2 Include, pure $ f3 Include, f4]
        )
  @?= CriteriaFailure "bad"
  , testCase "measuring attrition that includes a failure"
  $  toExpectedAttrition (measureSubjectAttrition
        (Just
          ( into @Criteria
          $ pure (f1 Include)
          : [pure $ f2 Include, pure $ f3 Include, f4]
          )
        )
        [ checkCohortStatus
          ( into @Criteria
          $ pure (f1 Include)
          : [pure $ f2 Include, pure $ f3 Include, f4]
          )
        , checkCohortStatus
          ( from @[CriterionThatCanFail]
          $ [pure $ f1 Include, pure $ f2 Include, pure $ f3 Include]
          )
        ])
  @?= (1, 2, [(CriteriaFailure "bad", 1), (Included, 1)])
  ]



