module Tests.Hasklepias
  ( tests
  ) where

import           Test.Tasty

import qualified Tests.AssessmentIntervals     as AI

tests :: IO ()
tests = defaultMain $ testGroup "Cohort module(s) tests" [AI.tests]
