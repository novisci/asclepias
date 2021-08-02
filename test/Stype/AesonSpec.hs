{-# LANGUAGE OverloadedStrings #-}
module Stype.AesonSpec (spec) where

import Stype
import Data.Aeson
import Test.Hspec

spec :: Spec
spec = do
  describe "encoding Continuous" $
    do
     it "Inf encodes" $ toJSON (ContInf :: Continuous Double) `shouldBe` "Inf"
     it "Negative Inf encodes" $ toJSON (NegContInf :: Continuous Double) `shouldBe` "-Inf" 
     it "Continuous encodes" $ toJSON (Cont 7.5 :: Continuous Double) `shouldBe` Number 7.5

  describe "encoding Nonnegative Continuous" $
    do
     it "NonNegative Inf encodes" $ toJSON (NonNegContInf :: NonnegContinuous Double) `shouldBe` "Inf" 
     it "Continuous encodes" $ toJSON (NonNegCont 7.5 :: NonnegContinuous Double) `shouldBe` Number 7.5

  describe "encoding EventTime" $
    do
     it "EventTime Inf encodes" $ toJSON (EventTime NonNegContInf :: EventTime Double) `shouldBe` "Inf" 
     it "EventTime encodes" $ toJSON (EventTime (NonNegCont 7.5) :: EventTime Double) `shouldBe` Number 7.5 

  describe "encoding Count" $
    do
     it "count encodes" $ toJSON (8 :: Count) `shouldBe` Number 8

  describe "encoding Binary" $
    do
     it "zero encodes" $ toJSON Zero `shouldBe` Bool False
     it "one encodes" $ toJSON One `shouldBe` Bool True

  describe "encoding Nominal" $ 
    do 
      it "nominal encodes" pending

  describe "encoding MaybeCensored" $ 
    do 
      it "MaybeCensored encodes" pending