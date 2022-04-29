{-
This module simply contains the tests for examples
that are currently undocumented.
-}
module UndocumentedExamples where

import           UndocumentedExamples.NegativeControl 
                                               as NC
import           UndocumentedExamples.CalendarCohort 
                                               as CC

import           Hasklepias

examples :: TestTree
examples = testGroup
  "Undocumented example tests"
  [NC.example, CC.example]
