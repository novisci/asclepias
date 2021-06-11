{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module FeatureCompose.AesonSpec (spec) where

import IntervalAlgebra
import EventData
import EventData.Context
import FeatureEvents
import FeatureCompose
import FeatureCompose.Aeson
import Data.Aeson (encode)
import Data.Maybe
import Data.Time as DT
import Test.Hspec ( shouldBe, it, Spec )
import qualified Data.ByteString.Lazy as B


ex1 :: Events Int
ex1 = [event (beginerval 10 0) (context Nothing (packConcepts ["enrollment"]))]

index:: (Ord a) =>
     Events a
  -> FeatureData (Interval a)
index es =
    case firstConceptOccurrence ["enrollment"] es of
        Nothing -> featureDataL (Other "No Enrollment")
        Just x  -> pure (getInterval x)


spec :: Spec
spec = do
    it "an Int event is parsed correctly" $
       encode (index ex1)  `shouldBe` "{\"end\":10,\"begin\":0}"


