module CohortExamples where

import           CohortExamples.Example1      as C1
import           Hasklepias

examples :: TestTree
examples = testGroup "blah" [C1.example]
