{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Cohort.OutputSpec (
  spec
 ) where

import Cohort
import Data.Aeson
import Data.HashMap.Internal as H
import Data.Vector as V (fromList)
import qualified Data.Set as Set (fromList)
import Test.Hspec ( describe, pending, shouldBe, it, Spec )
import qualified Data.ByteString.Lazy          as B
                                                ( ByteString )
import Features (emptyAttributes)

attr1 :: Maybe AttritionInfo
attr1 = 
  Just $ MkAttritionInfo 2 $
    Set.fromList [ uncurry MkAttritionLevel (ExcludedBy (1, "feat2"), 1)
             , uncurry MkAttritionLevel (Included, 1)]

cw1 :: B.ByteString
cw1 = "{\"contents\":{\"ids\":[\"a\",\"b\"],\"colData\":[[5,5],[true,true]],\"colAttributes\":[{\"name\":\"dummy\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Count\"},{\"name\":\"another\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Bool\"}]},\"tag\":\"CW\"}"

cw2 :: B.ByteString
cw2 = "{\"contents\":{\"ids\":[\"c\",\"d\"],\"colData\":[[6,8],[false,true]],\"colAttributes\":[{\"name\":\"dummy\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Count\"},{\"name\":\"another\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Bool\"}]},\"tag\":\"CW\"}"

rw1 :: B.ByteString
rw1 = "{\"contents\":{\"rowAttributes\":[{\"name\":\"dummy\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Count\"},{\"name\":\"another\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Bool\"}],\"rowData\":[[\"a\",[5,true]],[\"b\",[5,true]]]},\"tag\":\"RW\"}"

rw2 :: B.ByteString
rw2 = "{\"contents\":{\"rowAttributes\":[{\"name\":\"dummy\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Count\"},{\"name\":\"another\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Bool\"}],\"rowData\":[[\"c\",[6,false]],[\"d\",[8,true]]]},\"tag\":\"RW\"}"

ep :: Value
ep = toJSON emptyAttributes

cwt :: CohortDataShapeJSON 
cwt = CW $ MkColumnWiseJSON 
  [ Object $ H.fromList [("name", String "dummy")
                    , ("attrs", ep)
                    , ("type", String "Count")]
  , Object $ H.fromList [("name", String "another")
                    , ("attrs", ep)
                    , ("type", String "Bool")]
  ]
  [String "a", String "b", String "c", String "d"]  
  [ [Number 5, Number 5, Number 6, Number 8]
  , [Bool True, Bool True, Bool False, Bool True]
  ]

rwt :: CohortDataShapeJSON 
rwt = RW $ MkRowWiseJSON 
  [ Object $ H.fromList [("name", String "dummy")
                    , ("attrs", ep)
                    , ("type", String "Count")]
  , Object $ H.fromList [("name", String "another")
                    , ("attrs", ep)
                    , ("type", String "Bool")]
  ]
  [ Array $ V.fromList [String "a", Array $ V.fromList [ Number 5, Bool True]]
  , Array $ V.fromList [String "b", Array $ V.fromList [ Number 5, Bool True]]
  , Array $ V.fromList [String "c", Array $ V.fromList [ Number 6, Bool False]]
  , Array $ V.fromList [String "d", Array $ V.fromList [ Number 8, Bool True]]
  ]

cw1p2 :: B.ByteString 
cw1p2 = "{\"contents\":{\"ids\":[\"a\",\"b\",\"c\",\"d\"],\"colData\":[[5,5,6,8],[true,true,false,true]],\"colAttributes\":[{\"name\":\"dummy\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Count\"},{\"name\":\"another\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Bool\"}]},\"tag\":\"CW\"}"

rw1p2 :: B.ByteString
rw1p2 = "{\"contents\":{\"rowAttributes\":[{\"name\":\"dummy\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Count\"},{\"name\":\"another\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Bool\"}],\"rowData\":[[\"a\",[5,true]],[\"b\",[5,true]],[\"c\",[6,false]],[\"d\",[8,true]]]},\"tag\":\"RW\"}"
 

spec :: Spec
spec = do

  describe "Cohort I/O" $
    do

      it "AttritionInfo can roundtrip via JSON" $
        decode (encode attr1) `shouldBe` attr1
  describe "Cohort to/fromJSON" $
    do

      it "columnwise cohort data can be combined" $
         (decode cw1 <> decode cw2)  `shouldBe` Just cwt
      it "columnwise cohort data can be combined and serialized" $
         encode (decode cw1 <> decode cw2 :: Maybe CohortDataShapeJSON)  `shouldBe` cw1p2
      it "rowwise cohort data can be combined" $
         (decode rw1 <> decode rw2)  `shouldBe` Just rwt
      it "rowwise cohort data can be combined and serialized" $
         encode (decode rw1 <> decode rw2 :: Maybe CohortDataShapeJSON)  `shouldBe` rw1p2