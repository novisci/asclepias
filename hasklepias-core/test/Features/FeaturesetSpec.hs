{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Features.FeaturesetSpec
  ( spec
  ) where

import           Data.Aeson                     ( encode )
import qualified Data.ByteString.Lazy          as B
import           Data.List
import           Data.List.NonEmpty            as NE
-- import Data.Maybe
-- import Data.Time as DT
import           Data.Text                      ( Text )
import           Features
import           Test.Hspec                     ( Spec
                                                , it
                                                , pending
                                                , shouldBe
                                                )


s1f1 :: Feature "a" Bool
s1f1 = pure True

s2f1 :: Feature "a" Bool
s2f1 = pure False

instance HasAttributes "a" Bool where
  getAttributes x =
    MkAttributes "some Label" "longer label..." "a description" emptyPurpose

s1f2 :: Feature "b" (Maybe Text)
s1f2 = pure Nothing

s2f2 :: Feature "b" (Maybe Text)
s2f2 = pure (Just "bye")

instance HasAttributes "b" (Maybe Text) where
  getAttributes x = emptyAttributes

s1 :: Featureset
s1 = featureset (packFeature s1f1 :| [packFeature s1f2])

s2 :: Featureset
s2 = featureset (packFeature s2f1 :| [packFeature s2f2])

dt :: FeaturesetList
dt = MkFeaturesetList (s1 :| [s2])

-- tdt :: DataFrameShape
-- tdt = makeDataFrameReady dt


spec :: Spec
spec = do

  it "tdt encodes correctly" pending
        -- encode tdt `shouldBe`
        --     "{\"data\":[[true,false],[null,\"bye\"]],\
        --     \\"attributes\":[{\"name\":\"a\",\
        --     \\"type\":\"Bool\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"a description\",\"getLongLabel\":\"longer label...\",\"getShortLabel\":\"some Label\"}},\
        --     \{\"name\":\"b\",\"type\":\"Maybe Text\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"}}]}"
