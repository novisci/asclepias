module Examples
  ( examples
  ) where

import qualified FeatureExamples               as F
import qualified CohortExamples   as C
import           Hasklepias

examples :: IO ()
examples = defaultMain . testGroup "Examples" =<< sequenceA [pure F.examples, pure C.examples]
