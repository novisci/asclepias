module Examples
  ( examples
  ) where

import qualified CohortExamples       as C
import qualified FeatureExamples      as F
import           Hasklepias
import qualified UndocumentedExamples as U

examples :: IO ()
examples = defaultMain . testGroup "Examples" =<< sequenceA
  [pure F.examples, pure C.examples, pure U.examples]
