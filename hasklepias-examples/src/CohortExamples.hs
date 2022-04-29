module CohortExamples where

import           CohortExamples.CalendarCohort as C1
import           CohortExamples.DefineIndexSet as DI
-- import           CohortExamples.Minimal
--                                                as C2
import           Hasklepias

examples :: TestTree
examples = testGroup "Tests of cohort examples" [DI.example, C1.example]
