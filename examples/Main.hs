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
import Test.Hspec ( hspec )

main :: IO ()
main = hspec $ do exampleFeatures1Spec
                  exampleFeatures2Spec
                  exampleFeatures3Spec 