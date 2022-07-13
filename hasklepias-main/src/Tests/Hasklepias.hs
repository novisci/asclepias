module Tests.Hasklepias
  ( tests
  , benches
  ) where

import qualified Hasklepias.AppBuilder.ProcessLines.Tests
                                               as LF
import           Test.Tasty
import qualified Test.Tasty.Bench              as B
import qualified Tests.AssessmentIntervals     as AI

tests :: IO ()
tests = defaultMain $ testGroup "Cohort module(s) tests" [AI.tests, LF.tests]

benches :: IO ()
benches = B.defaultMain $ [B.bgroup "Hasklepias benchmarks" []]
-- LF.benches
