module CohortExamples where

import           CohortExamples.DefineIndexSet as DI
import           CohortExamples.ExampleCalendarCohort
                                               as C1
-- import           CohortExamples.Minimal
--                                                as C2
import           Hasklepias

examples :: TestTree
examples = testGroup "Tests of cohort examples" [DI.example, C1.example]
