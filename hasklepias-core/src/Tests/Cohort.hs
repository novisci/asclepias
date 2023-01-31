module Tests.Cohort
  ( tests
  ) where

import           Test.Tasty
import qualified Tests.Cohort.Cohort   as CH
import qualified Tests.Cohort.Core     as C
import qualified Tests.Cohort.Criteria as CR
import qualified Tests.Cohort.Output   as O

tests :: TestTree
tests = testGroup "Cohort module(s) tests" [C.tests, CH.tests, CR.tests, O.tests]
