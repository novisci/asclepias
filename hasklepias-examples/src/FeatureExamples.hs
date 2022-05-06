module FeatureExamples where

import           FeatureExamples.Example1      as F1
import           FeatureExamples.Example2      as F2
import           FeatureExamples.Example3      as F3
import           FeatureExamples.Example4      as F4
import           Hasklepias

examples :: TestTree
examples = testGroup "blah" [F1.example, F2.example, F3.example, F4.example]
