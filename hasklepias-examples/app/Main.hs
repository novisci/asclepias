module Main
  ( module ExampleFeatures1
  , main
  ) where

import           ExampleCohort1                 ( exampleCohort1tests )
import           ExampleFeatures1               ( exampleFeatures1Spec )
import           ExampleFeatures2               ( durationOfHospitalizedAntibiotics
                                                )
import           ExampleFeatures3               ( exampleFeatures3Spec )
import           ExampleFeatures4               ( exampleFeatures4Spec )
import           Test.Tasty
import           Test.Tasty.Hspec               ( testSpec )

-- NOTE: testSpec is used because the project orginally used the Hspec testing 
-- framework. We have since moved to Tasty. The tests in exampleFeatures(1-3)Spec
-- should be updated to Tasty at some point. 
main :: IO ()
main = do
  spec1 <- testSpec "spec1" exampleFeatures1Spec
  -- TODO
  -- spec2 <- testSpec "spec2" durationOfHospitalizedAntibiotics
  spec3 <- testSpec "spec3" exampleFeatures3Spec
  spec4 <- testSpec "spec4" exampleFeatures4Spec
  defaultMain
    (testGroup "tests"
      -- [spec1, spec2, spec3, spec4, testGroup "Tests" [exampleCohort1tests]]
               [spec1, spec3, spec4, testGroup "Tests" [exampleCohort1tests]]
    )
