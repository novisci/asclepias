module FeatureExamples where


import           FeatureExamples.DurationsWithMultipleConditions
                                               as F1
import           FeatureExamples.HistoryOfEvent
                                               as F2
import           FeatureExamples.LastEventInWindow
                                               as F3
-- import           FeatureExamples.Example4      as F4
import           Hasklepias

examples :: TestTree
examples =
  testGroup "Feature example tests" [F1.example, F2.example, F3.example]
