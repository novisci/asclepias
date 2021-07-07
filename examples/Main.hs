module Main(
    module ExampleFeatures1
  , main
) where

import ExampleFeatures1 ( exampleFeatures1Spec )
import ExampleFeatures2 ( exampleFeatures2Spec ) 
import ExampleFeatures3 ( exampleFeatures3Spec ) 
import ExampleCohort1   ( exampleCohort1tests )
import Test.Tasty
import Test.Tasty.Hspec ( testSpec )
import Test.Hspec       ( hspec )

-- NOTE: testSpec is used because the project orginally used the Hspec testing 
-- framework. We have since moved to Tasty. The tests in exampleFeatures(1-3)Spec
-- should be updated to Tasty at some point. 
main :: IO ()
main = do
  spec1 <- testSpec "spec1" exampleFeatures1Spec
  spec2 <- testSpec "spec2" exampleFeatures2Spec
  spec3 <- testSpec "spec3" exampleFeatures3Spec 
  defaultMain
    (testGroup "tests"
      [ spec1
      , spec2
      , spec3 
      , testGroup "Tests" [exampleCohort1tests]
      ])
