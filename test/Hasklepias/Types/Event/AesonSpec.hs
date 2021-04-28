{-# LANGUAGE OverloadedStrings #-}
module Hasklepias.Types.Event.AesonSpec (spec) where

import IntervalAlgebra
import Hasklepias.Types.Event
import Hasklepias.Types.Event.Aeson
import Data.Aeson
import Data.Time as DT
import Hasklepias.Types.Context as HC
import Test.Hspec
import qualified Data.ByteString.Lazy as B

testInInt :: B.ByteString
testInInt = "[\"abc\", 0, 1, \"Diagnosis\",\
          \[\"someThing\"],\
          \{\"domain\":\"Diagnosis\",\"time\":{\"begin\":0,\"end\":1}}]"

testInputsInt :: B.ByteString
testInputsInt = 
      "[\"abc\", 0, 1, \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\"time\":{\"begin\":0,\"end\":1}}]\n\
      \[\"abc\", 5, 6, \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\"time\":{\"begin\":5,\"end\":6}}]"

testInDay :: B.ByteString
testInDay = "[\"abc\", \"2020-01-01\", \"2020-01-02\", \"Diagnosis\",\
          \[\"someThing\"],\
          \{\"domain\":\"Diagnosis\",\
          \ \"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-02\"}}]"


testInputsDay :: B.ByteString
testInputsDay = 
      "[\"abc\", \"2020-01-01\", \"2020-01-02\", \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\
      \ \"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-02\"}}]\n\
      \[\"abc\", \"2020-01-05\", \"2020-01-06\", \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\
      \ \"time\":{\"begin\":\"2020-01-05\",\"end\":\"2020-01-06\"}}]"

testOutInt1 = event (unsafeInterval (0 :: Int) (1 :: Int)) (HC.context ["someThing"])
testOutInt2 = event (unsafeInterval (5 :: Int) (6 :: Int)) (HC.context ["someThing"])

testOutDay1 = event (unsafeInterval (fromGregorian 2020 1 1) (fromGregorian 2020 1 2)) (HC.context ["someThing"])
testOutDay2 = event (unsafeInterval (fromGregorian 2020 1 5) (fromGregorian 2020 1 6)) (HC.context ["someThing"])


spec :: Spec 
spec = do 
    it "an Int event is parsed correctly" $ 
       ( decode testInInt )  `shouldBe` (Just testOutInt1)
    it "lines of Int events are parsed correctly" $
       (parseEventIntLines testInputsInt) `shouldBe` [testOutInt1, testOutInt2]

    it "a Day event is parsed correctly" $ 
       ( decode testInDay )  `shouldBe` (Just testOutDay1)
    it "lines of Int events are parsed correctly" $
       (parseEventDayLines testInputsDay) `shouldBe` [testOutDay1, testOutDay2]
