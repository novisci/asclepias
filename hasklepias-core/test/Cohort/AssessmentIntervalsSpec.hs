{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}


module Cohort.AssessmentIntervalsSpec (
  spec
 ) where

import IntervalAlgebra
import IntervalAlgebra.Arbitrary
import Cohort.Index
import Cohort.AssessmentIntervals
import Test.Hspec ( describe, pending, shouldBe, it, Spec )
import Test.QuickCheck

prop_baseline :: (IntervalSizeable a b) => b -> Interval a -> Property
prop_baseline dur i = relate (baseline dur (makeIndex i)) (makeIndex i) === Meets

prop_baselineFinishedBy :: (IntervalSizeable a b) => b -> Interval a -> Property
prop_baselineFinishedBy d i = relate (baselineFinishedBy d (makeIndex i)) (makeIndex i) === FinishedBy

prop_baselineBefore :: (IntervalSizeable a b) => b -> b -> Interval a -> Property
prop_baselineBefore s d i = relate (baselineBefore s d (makeIndex i)) (makeIndex i) === Before

prop_followup :: (IntervalSizeable a b) =>  b -> Interval a -> Property
prop_followup d i = relate (followup d (makeIndex i)) (makeIndex i) === StartedBy

prop_followupMetBy :: (IntervalSizeable a b) =>  b -> Interval a -> Property
prop_followupMetBy d i = relate (followupMetBy d (makeIndex i)) (makeIndex i) === MetBy 

prop_followupAfter :: (IntervalSizeable a b) => b -> b -> Interval a -> Property
prop_followupAfter s d i = relate (followupAfter s d (makeIndex i)) (makeIndex i) === After 

spec :: Spec
spec = do

  describe "Assessment Interval properties" $
    do

      it "baseline meets index" $ property ( prop_baseline @Int @Int)
      it "baselineFinishedBy finishes index" $ property ( prop_baselineFinishedBy @Int @Int)
      it "baselineBefore precedes index" $ property ( prop_baselineBefore @Int @Int)
      it "followup starts index" $ property ( prop_followup @Int @Int)
      it "followupMetBy metBy index" $ property ( prop_followupMetBy @Int @Int)
      it "followupAfter after index" $ property ( prop_followupAfter @Int @Int)