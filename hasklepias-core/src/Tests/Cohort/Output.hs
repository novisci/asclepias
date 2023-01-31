-- | TODO This module should test that the output shape behaves as expected.
-- The only meaningful thing to test at the moment is the conversion from
-- features into ColumnWiseJSON. The current tests amount to double-checking
-- that the default implementations of To/FromJSON work, by performing a
-- roundtrip of the values. Since we're not altering those conversions, I don't
-- think this is worth testing. A future refactor (very soon) will focus on
-- changing the output shape. Tests related to expected output should be made
-- at that time, to conform to whatever reqirements needed there.
-- Since the current tests do not test anything meaningful, I (bbrown) don't
-- feel the need to update them for this interim test and consider what exists
-- relevant only in that some values of bytestrings can be cannibalized for
-- genuine tests.

{-# LANGUAGE OverloadedStrings #-}

module Tests.Cohort.Output
  ( tests
  ) where

import           Test.Tasty

-- TODO this needs to be done based on a soon-to-happen refactor of the output
-- module.

  {-
attr1 :: Maybe AttritionInfo
attr1 =
  Just $ MkAttritionInfo 2 2 $ fromList [(ExcludedBy "feat2", 1), (Included, 1)]

cw1 :: B.ByteString
cw1 =
  "{\"contents\":{\"ids\":[\"a\",\"b\"],\"cohortData\":[[5,5],[true,true]],\"attributes\":[{\"name\":\"dummy\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Count\"},{\"name\":\"another\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Bool\"}]},\"tag\":\"CW\"}"

cw2 :: B.ByteString
cw2 =
  "{\"contents\":{\"ids\":[\"c\",\"d\"],\"cohortData\":[[6,8],[false,true]],\"attributes\":[{\"name\":\"dummy\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Count\"},{\"name\":\"another\",\"attrs\":{\"getPurpose\":{\"getTags\":[],\"getRole\":[]},\"getDerivation\":\"\",\"getLongLabel\":\"\",\"getShortLabel\":\"\"},\"type\":\"Bool\"}]},\"tag\":\"CW\"}"

ep :: Value
ep = toJSON emptyAttributes

cwt :: ColumnWiseJSON
cwt = MkColumnWiseJSON
  [ Object $ fromList
    [("name", String "dummy"), ("attrs", ep), ("type", String "Count")]
  , Object $ fromList
    [("name", String "another"), ("attrs", ep), ("type", String "Bool")]
  ]
  [String "a", String "b", String "c", String "d"]
  [ [Number 5, Number 5, Number 6, Number 8]
  , [Bool True, Bool True, Bool False, Bool True]
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

  {- TESTS -}

tests :: TestTree
tests = testGroup
  "Unit tests on Cohort.Output"
  [ testCase "AttritionInfo can roundtrip via JSON"
  $   decode (encode attr1)
  @?= attr1
  , testCase "columnwise cohort data can be combined"
  $  (decode cw1 <> decode cw2)
  @?= Just cwt
  , testCase "columnwise cohort data can be combined and serialized"
  $   decode (encode (decode cw1 <> decode cw2 :: Maybe CohortJSON))
  @?= decode @CohortJSON cw1p2
  ]

-}

  {- TESTS -}

tests :: TestTree
tests = testGroup
  "Unit tests on Cohort.Output"
  [ ]
