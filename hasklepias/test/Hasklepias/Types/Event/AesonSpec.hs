{-# LANGUAGE OverloadedStrings #-}
module Hasklepias.Types.Event.AesonSpec (spec) where

import IntervalAlgebra
import Hasklepias.Types.Event
import Hasklepias.Types.Event.Aeson
import Data.Aeson
import Hasklepias.Types.Context as HC
import Test.Hspec
import qualified Data.ByteString.Lazy as B

testIn :: B.ByteString
testIn = "[\"abc\", 0, 1, \"Diagnosis\",\
          \[\"someThing\"],\
          \{\"domain\":\"Diagnosis\",\"time\":{\"begin\":0,\"end\":1}}]"

testInputs :: B.ByteString
testInputs = 
      "[\"abc\", 0, 1, \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\"time\":{\"begin\":0,\"end\":1}}]\n\
      \[\"abc\", 5, 6, \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\"time\":{\"begin\":5,\"end\":6}}]"


testOut1 = event (unsafeInterval (0 :: Int) (1 :: Int)) (HC.context ["someThing"])
testOut2 = event (unsafeInterval (5 :: Int) (6 :: Int)) (HC.context ["someThing"])

spec :: Spec 
spec = do 
    it "an event is parsed correctly" $ 
       ( decode testIn )  `shouldBe` (Just testOut1)
    it "lines of events are parsed correctly" $
       (parseEventIntLines testInputs) `shouldBe` [testOut1, testOut2]
