{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Tests.AssessmentIntervals
  ( tests
  ) where

import           AssessmentIntervals
import           IntervalAlgebra
import           IntervalAlgebra.Arbitrary
import           Test.Tasty
import           Test.Tasty.QuickCheck

prop_baseline :: (IntervalSizeable a b) => b -> Interval a -> Property
prop_baseline dur i = relate (baseline dur i) i === Meets

prop_baselineFinishedBy
  :: (IntervalSizeable a b) => b -> Interval a -> Property
prop_baselineFinishedBy d i = relate (baselineFinishedBy d i) i === FinishedBy

prop_baselineBefore
  :: (IntervalSizeable a b) => b -> b -> Interval a -> Property
prop_baselineBefore s d i = relate (baselineBefore s d i) i === Before

prop_followup :: (IntervalSizeable a b) => b -> Interval a -> Property
prop_followup d i = relate (followup d i) i === StartedBy

prop_followupMetBy :: (IntervalSizeable a b) => b -> Interval a -> Property
prop_followupMetBy d i = relate (followupMetBy d i) i === MetBy

prop_followupAfter
  :: (IntervalSizeable a b) => b -> b -> Interval a -> Property
prop_followupAfter s d i = relate (followupAfter s d i) i === After

tests :: TestTree
tests = testProperties
  "Property tests of AssessmentIntervals"
  [ ("baseline meets index", property (prop_baseline @Int @Int))
  , ( "baselineFinishedBy finishes index"
    , property (prop_baselineFinishedBy @Int @Int)
    )
  , ("baselineBefore precedes index", property (prop_baselineBefore @Int @Int))
  , ("followup starts index"        , property (prop_followup @Int @Int))
  , ("followupMetBy metBy index"    , property (prop_followupMetBy @Int @Int))
  , ("followupAfter after index"    , property (prop_followupAfter @Int @Int))
  ]

