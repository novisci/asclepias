{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cohort.InputSpec (spec) where

import IntervalAlgebra
import EventData
import Cohort
import EventData.Context as HC
import EventData.Context.Domain
import qualified EventData.Context.Facts as F
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
      \ \"facts\":{\"code\":{\"code\":\"abc\"}},\
      \ \"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-01\"}}]\n\
      \[\"abc\", \"2020-01-05\", \"2020-01-06\", \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\
      \ \"facts\":{\"code\":{\"code\":\"abc\"}},\
      \ \"time\":{\"begin\":\"2020-01-05\",\"end\":\"2020-01-06\"}}]"


testInputsDay2 :: B.ByteString
testInputsDay2 =
      "[\"def\", \"2020-01-01\", null, \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\
      \ \"facts\":{\"code\":{\"code\":\"abc\"}},\
      \ \"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-01\"}}]\n\
      \[\"def\", \"2020-01-05\", null, \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\
      \ \"facts\":{\"code\":{\"code\":\"abc\"}},\
      \ \"time\":{\"begin\":\"2020-01-05\",\"end\":\"2020-01-06\"}}]"

testInputsDayBad :: B.ByteString
testInputsDayBad =
      "[\"ghi\", \"2020-01-01\", null, \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"facts\":{\"code\":{\"code\":\"abc\"}},\
      \ \"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-01\"}}]\n\
      \[\"ghi\", \"2020-01-05\", null, \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\
      \ \"facts\":{\"code\":{\"code\":\"abc\"}},\
      \ \"time\":{\"begin\":\"2020-01-05\",\"end\":\"2020-01-04\"}}]"

testInput = testInputsDay1 <> "\n" <> testInputsDay2

dx :: Domain
dx = Diagnosis
  (DiagnosisFacts { code     = F.Code { F.code = "abc", F.codebook = Nothing }
                  , claim    = Nothing
                  , location = Nothing
                  , provider = Nothing
                  , hospitalization = Nothing
                  }
  )

testOutDay1 = event (beginerval 1 (fromGregorian 2020 1 1))
               (HC.context  dx  (packConcepts ["someThing"]) Nothing)
testOutDay2 = event (beginerval 2 (fromGregorian 2020 1 5))
               (HC.context dx (packConcepts [ "someThing"]) Nothing)


testOutPop = MkPopulation [
          MkSubject ("abc", [testOutDay1, testOutDay2])
        , MkSubject ("def", [testOutDay1, testOutDay2])
      ]

spec :: Spec
spec = do
    it "a population is parsed" $
       parsePopulationDayLines testInput `shouldBe` ([], testOutPop)

    it "a population is parsed" $
       parsePopulationDayLines (testInput <> "\n" <> testInputsDayBad)`shouldBe`
        ([MkSubjectParseError  (5, "Error in $: key \"domain\" not found")
        , MkSubjectParseError (6, "Error in $: ParseErrorInterval \"2020-01-05<=2020-01-05\"")], testOutPop)