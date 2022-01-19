{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Features.OutputSpec
  ( spec
  ) where

import           Data.Aeson                     ( encode )
import qualified Data.ByteString.Lazy          as B
import           Data.Maybe
import           Data.Time                     as DT
import           EventData
import           EventData.Context
import           EventData.Context.Domain
import           Features
import           Hasklepias.FeatureEvents
import           IntervalAlgebra
import           Test.Hspec                     ( Spec
                                                , it
                                                , shouldBe
                                                )


ex1 :: Events Int
ex1 =
  [ event
      (beginerval 10 0)
      (context (UnimplementedDomain ()) (packConcepts ["enrollment"]) Nothing)
  ]

index :: (Ord a) => Events a -> FeatureData (Interval a)
index es = case firstConceptOccurrence ["enrollment"] es of
  Nothing -> featureDataL (Other "No Enrollment")
  Just x  -> pure (getInterval x)

dummy :: Feature "dummy" Bool
dummy = pure True

instance HasAttributes "dummy" Bool where
  getAttributes x =
    MkAttributes "some Label" "longer label..." "a description" emptyPurpose

dummy2 :: Feature "dummy2" Bool
dummy2 = pure True

instance HasAttributes "dummy2" Bool where
  getAttributes x = emptyAttributes

spec :: Spec
spec = do
  it "an Int event is parsed correctly"
    $          encode (index ex1)
    `shouldBe` "{\"begin\":0,\"end\":10}"

  -- TODO: these are failing in CI because the `type' field is sorting to after
  --       
  it "dummy encodes correctly"
    $ encode dummy
    `shouldBe` "{\
        \\"attrs\":{\
            \\"getDerivation\":\"a description\",\
            \\"getLongLabel\":\"longer label...\",\
            \\"getPurpose\":{\"getRole\":[],\"getTags\":[]},\
            \\"getShortLabel\":\"some Label\"\
            \},\
        \\"data\":true,\
        \\"name\":\"dummy\",\
        \\"type\":\"Bool\"\
        \}"

  it "dummy2 encodes correctly"
    $ encode dummy2
    `shouldBe` "{\
        \\"attrs\":{\
            \\"getDerivation\":\"\",\
            \\"getLongLabel\":\"\",\
            \\"getPurpose\":{\"getRole\":[],\"getTags\":[]},\
            \\"getShortLabel\":\"\"\
            \},\
        \\"data\":true,\
        \\"name\":\"dummy2\",\
        \\"type\":\"Bool\"\
        \}"
