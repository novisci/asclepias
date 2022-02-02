{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Tests.Cohort.Criteria
  ( tests
  ) where

import           Cohort.Criteria
import           Cohort.Index
import           Data.List.NonEmpty
import           Features
import           IntervalAlgebra
import           Test.Tasty
import           Test.Tasty.HUnit

f1 :: Status -> Criterion
f1 s = criterion (makeFeature (featureDataR s) :: Feature "f1" Status)

f2 :: Status -> Criterion
f2 s = criterion (makeFeature (featureDataR s) :: Feature "f2" Status)

f3 :: Status -> Criterion
f3 s = criterion (makeFeature (featureDataR s) :: Feature "f3" Status)

f4 :: Criterion
f4 = criterion
  (makeFeature (featureDataL $ Other "something") :: Feature "f4" Status)

index :: Maybe (Index Interval Int)
index = Just $ makeIndex (beginerval 1 1)

tests :: TestTree
tests = testGroup
  "Unit tests on Cohort.Criteria"
  [ testCase "noIndex "
  $   checkCohortStatus Nothing (criteria $ pure (f1 Include))
  @?= SubjectHasNoIndex
  , testCase "include f1"
  $   checkCohortStatus index (criteria $ pure (f1 Include))
  @?= Included
  , testCase "include f1, f2, f3"
  $ checkCohortStatus index (criteria $ f1 Include :| [f2 Include, f3 Include])
  @?= Included
  , testCase "exclude on f2"
  $   checkCohortStatus index (criteria $ f2 Exclude :| [f3 Include])
  @?= ExcludedBy (1, "f2")
  , testCase "exclude on f2"
  $ checkCohortStatus index (criteria $ f1 Include :| [f2 Exclude, f3 Include])
  @?= ExcludedBy (2, "f2")
  , testCase "error on f4"
  $   checkCohortStatus index
                        (criteria $ f1 Include :| [f2 Include, f3 Include, f4])
  @?= ExcludedBy (4, "f4")
  ]



