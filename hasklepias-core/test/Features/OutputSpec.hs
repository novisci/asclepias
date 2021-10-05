{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Features.OutputSpec (spec) where

import IntervalAlgebra
import EventData
import EventData.Context
import Hasklepias.FeatureEvents
import Features
import Data.Aeson (encode)
import Data.Maybe
import Data.Time as DT
import Test.Hspec ( shouldBe, it, Spec )
import qualified Data.ByteString.Lazy as B
import EventData.Context.Domain


ex1 :: Events Int
ex1 = [event (beginerval 10 0) (context (UnimplementedDomain ()) (packConcepts ["enrollment"]) Nothing)]

index:: (Ord a) =>
     Events a
  -> FeatureData (Interval a)
index es =
    case firstConceptOccurrence ["enrollment"] es of
        Nothing -> featureDataL (Other "No Enrollment")
        Just x  -> pure (getInterval x)

dummy ::  Feature "dummy" Bool
dummy = pure True

instance HasAttributes "dummy" Bool where
    getAttributes x = MkAttributes "some Label" "longer label..." "a description" emptyPurpose

dummy2 ::  Feature "dummy2" Bool
dummy2 = pure True

instance HasAttributes "dummy2" Bool where
    getAttributes x = emptyAttributes 

spec :: Spec
spec = do
    it "an Int event is parsed correctly" $
       encode (index ex1)  `shouldBe` "{\"end\":10,\"begin\":0}"

    -- TODO: these are failing in CI because the `type' field is sorting to after
    --       
    it "dummy encodes correctly" $
        encode dummy `shouldBe` 
        "{\"data\":true,\"name\":\"dummy\",\
        \\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\
        \\"getDerivation\":\"a description\",\
        \\"getLongLabel\":\"longer label...\",\
        \\"getShortLabel\":\"some Label\"},\
        \\"type\":\"Bool\"}"

    it "dummy2 encodes correctly" $
        encode dummy2 `shouldBe` 
        "{\"data\":true,\"name\":\"dummy2\",\
        \\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\
        \\"getDerivation\":\"\",\
        \\"getLongLabel\":\"\",\
        \\"getShortLabel\":\"\"},\
        \\"type\":\"Bool\"}"
