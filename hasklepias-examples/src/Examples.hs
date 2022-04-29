module Examples
  ( examples
  ) where

import qualified CohortExamples                as C
import qualified FeatureExamples               as F
import qualified UndocumentedExamples          as U
import           Hasklepias

examples :: IO ()
examples = defaultMain . testGroup "Examples" =<< sequenceA
  [pure F.examples, pure C.examples, pure U.examples]
