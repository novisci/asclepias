{-# LANGUAGE OverloadedStrings #-}
module Stype.Tests.Aeson
  ( tests
  ) where

import           Data.Aeson
import           Stype
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup
  "stype aeson tests"
  [ testGroup
    "encoding Continuous"
    [ testCase "Inf encodes" $ toJSON (ContInf :: Continuous Double) @?= "Inf"
    , testCase "Negative Inf encodes"
    $   toJSON (NegContInf :: Continuous Double)
    @?= "-Inf"
    , testCase "Continuous encodes"
    $   toJSON (Cont 7.5 :: Continuous Double)
    @?= Number 7.5
    ]
  , testGroup
    "encoding Nonnegative Continuous"
    [ testCase "NonNegative Inf encodes"
    $   toJSON (NonNegContInf :: NonnegContinuous Double)
    @?= "Inf"
    , testCase "Continuous encodes"
    $   toJSON (NonNegCont 7.5 :: NonnegContinuous Double)
    @?= Number 7.5
    ]
  , testGroup
    "encoding EventTime"
    [ testCase "EventTime Inf encodes"
    $   toJSON (EventTime NonNegContInf :: EventTime Double)
    @?= "Inf"
    , testCase "EventTime encodes"
    $   toJSON (EventTime (NonNegCont 7.5) :: EventTime Double)
    @?= Number 7.5
    ]
  , testGroup "encoding Count"
              [testCase "count encodes" $ toJSON (8 :: Count) @?= Number 8]
  , testGroup
    "encoding Binary"
    [ testCase "zero encodes" $ toJSON Zero @?= Bool False
    , testCase "one encodes" $ toJSON One @?= Bool True
    ]
  -- testGroup "encoding Nominal" [
  --   testCase "nominal encodes" pending
  -- ],
  -- testGroup "encoding MaybeCensored" [
  --   testCase "MaybeCensored encodes" pending
  -- ]
  ]
