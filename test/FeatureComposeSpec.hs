{-# LANGUAGE OverloadedStrings #-}

module FeatureComposeSpec (spec) where

import FeatureCompose
import Control.Applicative
import Test.Hspec ( describe, pending, shouldBe, it, Spec )
import Data.Foldable

d1 :: FeatureDefinition (FeatureData Int) Int
d1 = defineM
  (\x ->
    if x < 0 then
      featureDataL $ Other "at least 1 < 0"
    else
      pure (x + 1)
   )

d2 :: FeatureDefinition (FeatureData Int) Int
d2 = define (*2)

d3 ::  FeatureDefinition (FeatureData Int, FeatureData Int) Int
d3 = define2 (+)

spec :: Spec
spec = do

  describe "checking d1" $
    do

      it "eval of d1 returns correct values for List" $
        eval d1 (featureDataR 5) `shouldBe`
          featureDataR 6
      it "d1 returns correct error for List" $
        eval d1 (featureDataR (-1)) `shouldBe`
          (featureDataL (Other "at least 1 < 0") :: (FeatureData Int))

  describe "checking d2" $
    do
      it "eval of d2 returns correct values for List" $
        eval d2 (featureDataR 5) `shouldBe`
          featureDataR 10
      it "eval of d2 returns correct values for List" $
        eval d2 (featureDataR 5) `shouldBe`
          featureDataR 10
      it "d2 returns correct error for List" $
        eval d2 (featureDataL (Other "at least 1 < 0")) `shouldBe`
          (featureDataL (Other "at least 1 < 0") :: (FeatureData Int))

  describe "checking d3" $
    do
      it "eval of d2 returns correct values for List" $
        eval d3 (featureDataR 5, featureDataR 6)  `shouldBe`
          featureDataR 11
      it "eval of d3 returns correct values for List" $
        eval d3 (featureDataR 5, featureDataR 5) `shouldBe`
          featureDataR 10
      it "d3 returns correct error for List" $
        eval d3 (featureDataL (Other "at least 1 < 0"), featureDataR 6)  `shouldBe`
          (featureDataL (Other "at least 1 < 0") :: (FeatureData Int))
      it "d3 returns correct error for List" $
        eval d3 (featureDataR 6, featureDataL (Other "at least 1 < 0")) `shouldBe`
          (featureDataL (Other "at least 1 < 0") :: (FeatureData Int))
