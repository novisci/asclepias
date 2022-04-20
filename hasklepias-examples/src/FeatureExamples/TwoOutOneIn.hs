{-# LANGUAGE DataKinds #-}
module FeatureExamples.TwoOutOneIn where

import           CohortExamples.CreateAssessmentInterval
                                                ( bline )
import           ExampleEvents
import           Hasklepias

-- | Defines a feature that returns 'True' ('False' otherwise) if either:
--   * at least 1 event during the baseline interval has any of the 'cpts1' concepts
--   * there are at least 2 events that have 'cpts2' concepts which have at least
--     7 days between them during the baseline interval
{- tag::function[] -}
twoOutOneIn
  :: (IntervalSizeable a b)
  => [Text] -- ^ cpts1
  -> [Text] -- ^ cpts2
  -> Definition
       (  Feature "calendarIndex" (Interval a)
       -> Feature "allEvents" [Event Text ExampleModel a]
       -> Feature name Bool
       )
twoOutOneIn cpts1 cpts2 = buildNofXOrMofYWithGapBool 1
                                                     (containsConcepts cpts1)
                                                     1
                                                     7
                                                     bline
                                                     concur
                                                     (containsConcepts cpts2)
{-  end::function[] -}
