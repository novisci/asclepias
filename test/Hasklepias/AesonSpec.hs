{-# LANGUAGE OverloadedStrings #-}
module Hasklepias.AesonSpec (spec) where

import IntervalAlgebra
import EventData
import Hasklepias.Aeson
import Hasklepias.Cohort
import EventData.Context as HC
import EventData.Context.Domain
import Data.Aeson
import Data.Maybe
import Data.Time as DT
import Test.Hspec
import qualified Data.ByteString.Lazy as B



testInputsDay1 :: B.ByteString
testInputsDay1 = 
      "[\"abc\", \"2020-01-01\", \"2020-01-02\", \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\
      \ \"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-02\"}}]\n\
      \[\"abc\", \"2020-01-05\", \"2020-01-06\", \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\
      \ \"time\":{\"begin\":\"2020-01-05\",\"end\":\"2020-01-06\"}}]"


testInputsDay2 :: B.ByteString
testInputsDay2 = 
      "[\"def\", \"2020-01-01\", null, \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\
      \ \"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-02\"}}]\n\
      \[\"def\", \"2020-01-05\", null, \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\
      \ \"time\":{\"begin\":\"2020-01-05\",\"end\":\"2020-01-06\"}}]"

testInput = testInputsDay1 <> "\n" <> testInputsDay2

testOutDay1 = event (beginerval 1 (fromGregorian 2020 1 1))
               (HC.context ( Just $ UnimplementedDomain () ) (packConcepts ["someThing"]))
testOutDay2 = event (beginerval 1 (fromGregorian 2020 1 5)) 
               (HC.context ( Just $ UnimplementedDomain () ) (packConcepts [ "someThing"]))


testOutPop = MkPopulation [
          MkSubject ("abc", [testOutDay1, testOutDay2])
        , MkSubject ("def", [testOutDay1, testOutDay2])
      ]

spec :: Spec 
spec = do 
    it "a population is parsed" $ 
       ( parsePopulationDayLines testInput ) `shouldBe` (testOutPop)
