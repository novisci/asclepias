module FeatureExamples where


import           FeatureExamples.CountOfHospitalEvents
                                               as CH
import           FeatureExamples.DrugDiscontinuation
                                               as DD
import           FeatureExamples.DurationsWithMultipleConditions
                                               as DC
import           FeatureExamples.HistoryOfEvent
                                               as HE
import           FeatureExamples.LastEventInWindow
                                               as LW
import           FeatureExamples.TwoOutOneIn   as TOI

import           Hasklepias

examples :: TestTree
examples = testGroup
  "Feature example tests"
  [DC.example, HE.example, LW.example, CH.example, DD.example, TOI.example]
