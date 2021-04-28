module Main(
    module Hasklepias
  , module ExampleEvents
  , module ExampleFeatures1
  , main
) where

import Hasklepias
import ExampleEvents
import ExampleFeatures1
import Test.Hspec ( hspec )

main :: IO ()
main = hspec exampleFeatures1Spec