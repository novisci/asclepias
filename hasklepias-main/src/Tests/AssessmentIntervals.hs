{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Tests.AssessmentIntervals
  ( tests
  ) where

import           EventDataTheory
import           Hasklepias.AssessmentIntervals
import           Test.Tasty
import           Test.Tasty.QuickCheck

prop_baseline :: (iv ~ Interval a, Ord a, SizedIv iv) => Moment iv -> iv -> Property
prop_baseline dur i = relate (baselineMeets dur i) i === Meets

prop_baselineFinishedBy
  :: (iv ~ Interval a, Ord a, SizedIv iv) => Moment iv -> iv -> Property
prop_baselineFinishedBy d i = relate (baselineFinishedBy d i) i === FinishedBy

prop_baselineBefore
  :: (iv ~ Interval a, Ord a, SizedIv iv) => Moment iv -> Moment iv -> iv -> Property
prop_baselineBefore s d i = relate (baselineBefore s d i) i === Before

prop_followup :: (iv ~ Interval a, Ord a, Ord (Moment iv), Num (Moment iv), SizedIv iv) => 
  Moment iv -> iv -> Property
prop_followup d i = relate (followup d i) i === StartedBy

prop_followupMetBy :: (iv ~ Interval a, Ord a, SizedIv iv) => Moment iv -> iv -> Property
prop_followupMetBy d i = relate (followupMetBy d i) i === MetBy

prop_followupAfter
  :: (iv ~ Interval a, Ord a, SizedIv iv) => Moment iv -> Moment iv -> iv -> Property
prop_followupAfter s d i = relate (followupAfter s d i) i === After

tests :: TestTree
tests = testProperties
  "Property tests of AssessmentIntervals"
  [ ("baseline meets index", property (prop_baseline @(Interval Int)))
  , ( "baselineFinishedBy finishes index"
    , property (prop_baselineFinishedBy @(Interval Int))
    )
  , ("baselineBefore precedes index", property (prop_baselineBefore @(Interval Int)))
  , ("followup starts index"        , property (prop_followup @(Interval Int)))
  , ("followupMetBy metBy index"    , property (prop_followupMetBy @(Interval Int)))
  , ("followupAfter after index"    , property (prop_followupAfter @(Interval Int)))
  ]

