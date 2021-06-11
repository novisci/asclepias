module Main(
    module Hasklepias
  , module ExampleEvents
  , module ExampleFeatures1
  , main
) where

import Hasklepias
import ExampleEvents
import ExampleFeatures1
import ExampleFeatures2
import ExampleFeatures3
import ExampleCohort1
import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec ( hspec )

-- main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [exampleCohort1tests]

main = do
  spec <- testSpec "spec" exampleFeatures1Spec
  defaultMain
    (testGroup "tests"
      [ spec
      , tests
      ])


-- main :: IO ()
-- main = hspec $ do exampleFeatures1Spec
--                   exampleFeatures2Spec
--                   exampleFeatures3Spec
--                   exampleCohort1tests 