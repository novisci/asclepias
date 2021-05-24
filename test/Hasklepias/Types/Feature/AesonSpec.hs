{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module Hasklepias.Types.Feature.AesonSpec (spec) where

import IntervalAlgebra
import Hasklepias.Types
import Hasklepias.Functions
import Hasklepias.Types.Feature.Aeson
import Data.Aeson
import Data.Time as DT
-- import Hasklepias.Types.Context as HC
import Test.Hspec ( shouldBe, it, Spec )
import qualified Data.ByteString.Lazy as B


ex1 :: Events Int
ex1 = [event (beginerval 10 0) (context $ packConcepts ["enrollment"])]

index:: (Intervallic Interval a) =>
     Events a
  -> FeatureData (Interval a)
index es =
    case firstConceptOccurrence ["enrollment"] es of
        Nothing -> featureDataL (Other "No Enrollment")
        Just x  -> featureDataR (getInterval x)


spec :: Spec
spec = do
    it "an Int event is parsed correctly" $
       encode (index ex1)  `shouldBe` "{\"end\":10,\"begin\":0}"


