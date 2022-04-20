module CohortExamples.CreateAssessmentInterval where

import           Hasklepias

{-  
The baseline interval is the interval (b - 60, b), where b is the begin of 
index. Here, baseline is defined as function that takes a filtration function
as an argument, so that the baseline FeatureData can be used to filter events
based on different predicate functions.
-}
{- tag::baseline[] -}
bline :: (IntervalSizeable a b) => Interval a -> AssessmentInterval a
bline = makeBaselineFromIndex 60
{- end::baseline[] -}

{- tag::followup[] -}
flwup :: (IntervalSizeable a b) => Interval a -> AssessmentInterval a
flwup = makeFollowupFromIndex 30
{- end::followup[] -}
