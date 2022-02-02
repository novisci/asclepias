module Tests.Cohort
  ( tests
  ) where

import           Test.Tasty
import qualified Tests.Cohort.AssessmentIntervals
                                               as AI
import qualified Tests.Cohort.Attrition        as AT

import qualified Tests.Cohort.Core             as C
import qualified Tests.Cohort.Criteria         as CR
import qualified Tests.Cohort.Output           as O
tests :: TestTree
tests = testGroup "Cohort module(s) tests"
                  [AI.tests, AT.tests, C.tests, CR.tests, O.tests]
