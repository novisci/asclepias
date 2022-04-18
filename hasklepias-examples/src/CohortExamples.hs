module CohortExamples where

import           CohortExamples.ExampleCalendarCohort
                                               as C1
import           Hasklepias

examples :: TestTree
examples = testGroup "blah" [C1.example]
