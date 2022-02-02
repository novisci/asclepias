module Tests
  ( coreTests
  ) where

import           Test.Tasty
import qualified Tests.Cohort                  as C
import qualified Tests.Features                as F

coreTests :: IO ()
coreTests = defaultMain . testGroup "Hasklepias core tests" =<< sequenceA
  [pure F.tests, pure C.tests]
