{-# LANGUAGE OverloadedStrings #-}

module Hasklepias.Types.FeatureSpec (spec) where

import Hasklepias.Types.Feature
import Control.Applicative
import Test.Hspec ( describe, pending, shouldBe, it, Spec )
import Data.Foldable

d1 :: FeatureDefinition  * Int Int
d1 = defineM
  (\x -> 
    if x < 0 then 
      featureDataL $ Other "at least 1 < 0"
    else 
      pure (x + 1)
   )

d2 :: FeatureDefinition  * Int Int
d2 = define (*2)

d3 ::  FeatureDefinition  Int Int Int
d3 = define2 (+)

spec :: Spec
spec = do 

  describe "checking d1" $ 
    do 

      it "eval of d1 returns correct values for List" $
        eval1 d1 (featureDataR [5, 6, 7]) `shouldBe`
          featureDataR [6, 7, 8]
      it "d1 returns correct error for List" $
        eval1 d1 (featureDataR [-1, 5, 6]) `shouldBe`
          (featureDataL (Other "at least 1 < 0") :: (FeatureData Int))

  describe "checking d2" $
    do 
      it "eval of d2 returns correct values for List" $
        eval1 d2 (featureDataR [5]) `shouldBe`
          featureDataR [10]
      it "eval of d2 returns correct values for List" $
        eval1 d2 (featureDataR [5, 6, 7]) `shouldBe`
          featureDataR [10, 12, 14]
      it "d2 returns correct error for List" $
        eval1 d2 (featureDataL (Other "at least 1 < 0")) `shouldBe`
          (featureDataL (Other "at least 1 < 0") :: (FeatureData Int))

  describe "checking d3" $
    do
      it "eval of d2 returns correct values for List" $
        eval2 d3 (featureDataR [5]) (featureDataR [6])  `shouldBe`
          featureDataR [11]
      it "eval of d3 returns correct values for List" $
        eval2 d3 (featureDataR [5, 6, 7]) (featureDataR [5, 6, 7]) `shouldBe`
          featureDataR [10, 12, 14]
      it "d3 returns correct error for List" $
        eval2 d3 (featureDataL (Other "at least 1 < 0")) (featureDataR [6])  `shouldBe`
          (featureDataL (Other "at least 1 < 0") :: (FeatureData Int))
      it "d3 returns correct error for List" $
        eval2 d3 (featureDataR [6]) (featureDataL (Other "at least 1 < 0")) `shouldBe`
          (featureDataL (Other "at least 1 < 0") :: (FeatureData Int))
