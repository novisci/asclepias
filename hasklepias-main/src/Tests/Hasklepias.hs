module Tests.Hasklepias
  ( tests
  , benches
  ) where

import           Test.Tasty
import qualified Test.Tasty.Bench              as B
import qualified Tests.AppBuilder.LineFilterApp
                                               as LF
import qualified Tests.AssessmentIntervals     as AI

tests :: IO ()
tests = defaultMain $ testGroup "Cohort module(s) tests" [AI.tests]

benches :: IO ()
benches = B.defaultMain LF.benches
