{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Tests.Cohort.Output
  ( tests
  ) where

import           Cohort
import           Cohort.Attrition
import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
                                                ( ByteString )
import           Features                       ( emptyAttributes )
import           GHC.Exts                       ( IsList(..) )
import           Test.Tasty
import           Test.Tasty.HUnit

attr1 :: Maybe AttritionInfo
attr1 = Just $ MkAttritionInfo 2 $ fromList
  [MkAttritionLevel (ExcludedBy (1, "feat2")) 1, MkAttritionLevel Included 1]

cw1 :: B.ByteString
cw1 =
  "{\"contents\":{\"ids\":[\"a\",\"b\"],\"cohortData\":[[5,5],[true,true]],\"attributes\":[{\"name\":\"dummy\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Count\"},{\"name\":\"another\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Bool\"}]},\"tag\":\"CW\"}"

cw2 :: B.ByteString
cw2 =
  "{\"contents\":{\"ids\":[\"c\",\"d\"],\"cohortData\":[[6,8],[false,true]],\"attributes\":[{\"name\":\"dummy\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Count\"},{\"name\":\"another\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Bool\"}]},\"tag\":\"CW\"}"

rw1 :: B.ByteString
rw1 =
  "{\"contents\":{\"attributes\":[{\"name\":\"dummy\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Count\"},{\"name\":\"another\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Bool\"}],\"cohortData\":[[\"a\",[5,true]],[\"b\",[5,true]]]},\"tag\":\"RW\"}"

rw2 :: B.ByteString
rw2 =
  "{\"contents\":{\"attributes\":[{\"name\":\"dummy\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Count\"},{\"name\":\"another\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Bool\"}],\"cohortData\":[[\"c\",[6,false]],[\"d\",[8,true]]]},\"tag\":\"RW\"}"

ep :: Value
ep = toJSON emptyAttributes

cwt :: CohortDataShapeJSON
cwt = CW $ MkColumnWiseJSON
  [ Object $ fromList
    [("name", String "dummy"), ("attrs", ep), ("type", String "Count")]
  , Object $ fromList
    [("name", String "another"), ("attrs", ep), ("type", String "Bool")]
  ]
  [String "a", String "b", String "c", String "d"]
  [ [Number 5, Number 5, Number 6, Number 8]
  , [Bool True, Bool True, Bool False, Bool True]
  ]

rwt :: CohortDataShapeJSON
rwt = RW $ MkRowWiseJSON
  [ Object $ fromList
    [("name", String "dummy"), ("attrs", ep), ("type", String "Count")]
  , Object $ fromList
    [("name", String "another"), ("attrs", ep), ("type", String "Bool")]
  ]
  [ Array $ fromList [String "a", Array $ fromList [Number 5, Bool True]]
  , Array $ fromList [String "b", Array $ fromList [Number 5, Bool True]]
  , Array $ fromList [String "c", Array $ fromList [Number 6, Bool False]]
  , Array $ fromList [String "d", Array $ fromList [Number 8, Bool True]]
  ]


attr :: B.ByteString
attr =
  "{\"getDerivation\":\"\",\
        \\"getLongLabel\":\"\",\
        \\"getPurpose\":{\"getRole\":[],\"getTags\":[]},\
        \\"getShortLabel\":\"\"\
        \}"

v1 :: B.ByteString
v1 = "{\"attrs\":" <> attr <> ",\"name\":\"dummy\",\"type\":\"Count\"" <> "}"

v2 :: B.ByteString
v2 = "{\"attrs\":" <> attr <> ",\"name\":\"another\",\"type\":\"Bool\"" <> "}"

cw1p2 :: B.ByteString
cw1p2 =
  "{\"contents\":{\
          \\"attributes\":["
    <> v1
    <> ","
    <> v2
    <> "],\
          \\"cohortData\":[[5,5,6,8],[true,true,false,true]],\
          \\"ids\":[\"a\",\"b\",\"c\",\"d\"]},\
          \\"tag\":\"CW\"}"

rw1p2 :: B.ByteString
rw1p2 =
  "{\"contents\":{\
          \\"attributes\":["
    <> v1
    <> ","
    <> v2
    <> "],\
          \\"cohortData\":[[\"a\",[5,true]],[\"b\",[5,true]],[\"c\",[6,false]],[\"d\",[8,true]]]\
          \},\
          \\"tag\":\"RW\"}"


tests :: TestTree
tests = testGroup
  "Unit tests on Cohort.Output"
  [ testCase "AttritionInfo can roundtrip via JSON"
  $   decode (encode attr1)
  @?= attr1
  , testCase "columnwise cohort data can be combined"
  $   (decode cw1 <> decode cw2)
  @?= Just cwt
  , testCase "columnwise cohort data can be combined and serialized"
  $   encode (decode cw1 <> decode cw2 :: Maybe CohortDataShapeJSON)
  @?= cw1p2
  , testCase "rowwise cohort data can be combined"
  $   (decode rw1 <> decode rw2)
  @?= Just rwt
  , testCase "rowwise cohort data can be combined and serialized"
  $   encode (decode rw1 <> decode rw2 :: Maybe CohortDataShapeJSON)
  @?= rw1p2
  ]
