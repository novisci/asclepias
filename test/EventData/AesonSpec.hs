{-# LANGUAGE OverloadedStrings #-}
module EventData.AesonSpec (spec) where

import IntervalAlgebra
import EventData
import EventData.Aeson
import EventData.Context as HC
import EventData.Context.Domain
import Data.Aeson
import Data.Maybe
import Data.Time as DT
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

testInDay2 :: B.ByteString
testInDay2 = "[\"abc\", \"2020-01-01\", null, \"Diagnosis\",\
          \[\"someThing\"],\
          \{\"domain\":\"Diagnosis\",\
          \ \"time\":{\"begin\":\"2020-01-01\",\"end\":null}}]"


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

testInputsDay2 :: B.ByteString
testInputsDay2 =
      "[\"abc\", \"2020-01-01\", null, \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\
      \ \"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-02\"}}]\n\
      \[\"abc\", \"2020-01-05\", null, \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\
      \ \"time\":{\"begin\":\"2020-01-05\",\"end\":\"2020-01-06\"}}]"

testOutInt1 = event (beginerval 1 (0 :: Int)) (HC.context ( UnimplementedDomain () ) (packConcepts ["someThing"]))
testOutInt2 = event (beginerval 1 (5 :: Int)) (HC.context ( UnimplementedDomain () ) (packConcepts ["someThing"]))

testOutDay1 = event (beginerval 1 (fromGregorian 2020 1 1))
                     (HC.context (UnimplementedDomain () ) (packConcepts ["someThing"]))
testOutDay2 = event (beginerval 1 (fromGregorian 2020 1 5))
               (HC.context (  UnimplementedDomain () ) (packConcepts [ "someThing"]))


dmo :: Domain
dmo = Demographics $ DemographicsFacts (DemographicsInfo BirthYear (Just "1987"))

enl :: Domain 
enl = Enrollment (EnrollmentFacts ())

jsonDemoTest :: B.ByteString
jsonDemoTest = "{\"domain\":\"Demographics\",\"facts\":{\"demo\":{\"field\":\"BirthYear\",\"info\":\"1987\"}}}"

jsonEnrollTest :: B.ByteString
jsonEnrollTest = "{\"domain\":\"Enrollment\",\"facts\":{\"plan\":{\"exchange\":\"Medicare\"}}}"


jsonOtherTest :: B.ByteString
jsonOtherTest = "{\"domain\":\"Labs\",\"facts\":{\"code\":{\"code\":\"XYZ\"}}}"

spec :: Spec
spec = do
    it "an Int event is parsed correctly" $
       decode testInInt  `shouldBe` Just testOutInt1
    it "lines of Int events are parsed correctly" $
       parseEventIntLines testInputsInt `shouldBe` ([], [testOutInt1, testOutInt2])

    it "a Day event is parsed correctly" $
       decode testInDay  `shouldBe` Just testOutDay1
    it "a Day event with missing end day is parsed correctly" $
       decode testInDay2  `shouldBe` Just testOutDay1
    it "lines of Int events are parsed correctly" $
       parseEventDayLines testInputsDay `shouldBe` ([], [testOutDay1, testOutDay2])
    it "jsonDemoTest is parsed correctly" $
       decode jsonDemoTest  `shouldBe` Just dmo
    it "jsonEnrollTest is parsed correctly" $
       decode jsonEnrollTest  `shouldBe` Just enl
    it "jsonOtherTest is parsed correctly" $
       decode jsonOtherTest  `shouldBe` Just (UnimplementedDomain ())