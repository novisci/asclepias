module Tests.Features
  ( tests
  ) where

import           Test.Tasty
import qualified Tests.Features.Compose        as C
import qualified Tests.Features.Output         as O

tests :: TestTree
tests = testGroup "Features module(s) tests" [C.tests, O.tests]
