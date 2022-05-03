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
  => [Text] -- ^ inpatientConcepts
  -> [Text] -- ^ outpatientConcepts 
  -> Definition -- <1>
       (  Feature "index" (Interval a)
       -> Feature "allEvents" [Event Text ExampleModel a]
       -> Feature name Bool
       )
twoOutOneIn inpatientConcepts outpatientConcepts = buildNofXOrMofYWithGapBool -- <2>
  1
  (containsConcepts inpatientConcepts) -- <3>
  1
  7
  (containsConcepts outpatientConcepts) -- <4>
  concur
  (makeBaselineFromIndex 10) -- <5>
{-  end::definition[] -}

ev i c = event i (context (packConcepts [c]) Medical Nothing)

case1 :: [ExampleEvent]
case1 = [ev (beginerval 1 5) "c1"]

case2 :: [ExampleEvent]
case2 = [ev (beginerval 1 1) "c2", ev (beginerval 1 9) "c2"]

case3 :: [ExampleEvent]
case3 = [ev (beginerval 1 1) "c2", ev (beginerval 1 8) "c2"]

case4 :: [ExampleEvent]
case4 = [ev (beginerval 1 11) "c1", ev (beginerval 1 8) "c2"]

example :: TestTree
example = testGroup
  "TwoOutOneIn tests"
  [ testCase "case 1"
  $   eval (twoOutOneIn ["c1"] ["c2"]) (pure $ beginerval 10 10) (pure case1)
  @?= makeFeature @"foo" (pure True)
  , testCase "case 2"
  $   eval (twoOutOneIn ["c1"] ["c2"]) (pure $ beginerval 10 10) (pure case2)
  @?= makeFeature @"foo" (pure True)
  , testCase "case 3"
  $   eval (twoOutOneIn ["c1"] ["c2"]) (pure $ beginerval 10 10) (pure case3)
  @?= makeFeature @"foo" (pure False)
  , testCase "case 4"
  $   eval (twoOutOneIn ["c1"] ["c2"]) (pure $ beginerval 10 10) (pure case4)
  @?= makeFeature @"foo" (pure False)
  ]
