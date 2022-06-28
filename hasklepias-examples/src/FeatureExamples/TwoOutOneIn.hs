{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module FeatureExamples.TwoOutOneIn
  ( example
  ) where

import           CohortExamples.CreateAssessmentInterval
                                                ( bline )
import           ExampleEvents
import           Hasklepias

-- | Defines a feature that returns 'True' ('False' otherwise) if either:
--   * at least 1 event during the baseline interval has any of the 'cpts1' concepts
--   * there are at least 2 events that have 'cpts2' concepts which have at least
--     7 days between them during the baseline interval

{- tag::definition[] -}
twoOutOneIn
  :: (IntervalSizeable a b)
  => [Text] -- ^ inpatientTags
  -> [Text] -- ^ outpatientTags 
  -> Definition -- <1>
       (  Feature "index" (Interval a)
       -> Feature "allEvents" [Event Text ExampleModel a]
       -> Feature name Bool
       )
twoOutOneIn inpatientTags outpatientTags = buildNofXOrMofYWithGapBool -- <2>
  1
  (containsTag inpatientTags) -- <3>
  1
  7
  (containsTag outpatientTags) -- <4>
  concur
  (makeBaselineMeetsIndex 10) -- <5>
{-  end::definition[] -}

ev i t = event i (context (packTagSet [t]) Medical Nothing)

case1 :: [ExampleEvent]
case1 = [ev (beginerval 1 5) "t1"]

case2 :: [ExampleEvent]
case2 = [ev (beginerval 1 1) "t2", ev (beginerval 1 9) "t2"]

case3 :: [ExampleEvent]
case3 = [ev (beginerval 1 1) "t2", ev (beginerval 1 8) "t2"]

case4 :: [ExampleEvent]
case4 = [ev (beginerval 1 11) "t1", ev (beginerval 1 8) "ct2"]

example :: TestTree
example = testGroup
  "TwoOutOneIn tests"
  [ testCase "case 1"
  $   eval (twoOutOneIn ["t1"] ["t2"]) (pure $ beginerval 10 10) (pure case1)
  @?= makeFeature @"foo" (pure True)
  , testCase "case 2"
  $   eval (twoOutOneIn ["t1"] ["t2"]) (pure $ beginerval 10 10) (pure case2)
  @?= makeFeature @"foo" (pure True)
  , testCase "case 3"
  $   eval (twoOutOneIn ["t1"] ["t2"]) (pure $ beginerval 10 10) (pure case3)
  @?= makeFeature @"foo" (pure False)
  , testCase "case 4"
  $   eval (twoOutOneIn ["t1"] ["t2"]) (pure $ beginerval 10 10) (pure case4)
  @?= makeFeature @"foo" (pure False)
  ]
