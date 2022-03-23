module Examples
  ( examples
  ) where

-- import qualified CohortExamples                as C
import qualified FeatureExamples               as F
import           Hasklepias

examples :: IO ()
examples = defaultMain . testGroup "Examples" =<< sequenceA
  [pure F.examples
  -- , pure C.examples
  ]
