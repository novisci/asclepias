{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module EventData.AesonSpec
  ( spec
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import           Data.Maybe
import           Data.Time                     as DT
import           EventData
import           EventData.Aeson
import           EventData.Context             as HC
import           EventData.Context.Domain
import qualified EventData.Context.Facts       as F
import           IntervalAlgebra
import           Test.Hspec

testInInt :: B.ByteString
testInInt =
  "[\"abc\", 0, 1, \"Diagnosis\",\
             \[\"someThing\"],\
             \{\"domain\":\"Diagnosis\",\
             \ \"facts\":{\"code\":{\"code\":\"abc\"}},\
             \ \"time\":{\"begin\":0,\"end\":1}}]"

testInputsInt :: B.ByteString
testInputsInt =
  "[\"abc\", 0, 1, \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\
      \ \"facts\":{\"code\":{\"code\":\"abc\"}},\
      \ \"time\":{\"begin\":0,\"end\":1}}]\n\
      \[\"abc\", 5, 6, \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\", \
      \ \"facts\":{\"code\":{\"code\":\"abc\"}},\
      \ \"time\":{\"begin\":5,\"end\":6}}]"

testInDay :: B.ByteString
testInDay =
  "[\"abc\", \"2020-01-01\", \"2020-01-01\", \"Diagnosis\",\
          \[\"someThing\"],\
          \{\"domain\":\"Diagnosis\",\
          \ \"facts\":{\"code\":{\"code\":\"abc\"}},\
          \ \"source\":{\"table\":\"someTable\"},\
          \ \"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-01\"}}]"

testInDay2 :: B.ByteString
testInDay2 =
  "[\"abc\", \"2020-01-01\", null, \"Diagnosis\",\
          \[\"someThing\"],\
          \{\"domain\":\"Diagnosis\",\
          \ \"facts\":{\"code\":{\"code\":\"abc\"}},\
          \ \"source\":{\"table\":\"someTable\"},\
          \ \"time\":{\"begin\":\"2020-01-01\",\"end\":null}}]"


testInputsDay :: B.ByteString
testInputsDay =
  "[\"abc\", \"2020-01-01\", \"2020-01-02\", \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\
      \ \"facts\":{\"code\":{\"code\":\"abc\"}},\
      \ \"source\":{\"table\":\"someTable\"},\
      \ \"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-01\"}}]\n\
      \[\"abc\", \"2020-01-05\", \"2020-01-06\", \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\
      \ \"facts\":{\"code\":{\"code\":\"abc\"}},\
      \ \"time\":{\"begin\":\"2020-01-05\",\"end\":\"2020-01-06\"}}]"

testInputsDay2 :: B.ByteString
testInputsDay2 =
  "[\"abc\", \"2020-01-01\", null, \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\
      \ \"facts\":{\"code\":{\"code\":\"abc\"}},\
      \ \"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-01\"}}]\n\
      \[\"abc\", \"2020-01-05\", null, \"Diagnosis\",\
      \[\"someThing\"],\
      \{\"domain\":\"Diagnosis\",\
      \ \"facts\":{\"code\":{\"code\":\"abc\"}},\
      \ \"time\":{\"begin\":\"2020-01-05\",\"end\":\"2020-01-06\"}}]"

dx :: Domain
dx = Diagnosis
  (DiagnosisFacts { code     = F.Code { F.code = "abc", F.codebook = Nothing }
                  , claim    = Nothing
                  , location = Nothing
                  , provider = Nothing
                  , hospitalization = Nothing
                  }
  )

testOutInt1 = event (beginerval 2 (0 :: Int))
                    (HC.context dx (packConcepts ["someThing"]) Nothing)
testOutInt2 = event (beginerval 2 (5 :: Int))
                    (HC.context dx (packConcepts ["someThing"]) Nothing)

testOutDay1 = event
  (beginerval 1 (fromGregorian 2020 1 1))
  (HC.context
    dx
    (packConcepts ["someThing"])
    (Just $ Source { table  = "someTable"
                   , file   = Nothing
                   , row    = Nothing
                   , column = Nothing
                   }
    )
  )
testOutDay2 = event (beginerval 2 (fromGregorian 2020 1 5))
                    (HC.context dx (packConcepts ["someThing"]) Nothing)


dmo :: Domain
dmo =
  Demographics $ DemographicsFacts (DemographicsInfo BirthYear (Just "1987"))

jsonDemoTest :: B.ByteString
jsonDemoTest =
  "{\"domain\":\"Demographics\",\"facts\":{\"demo\":{\"field\":\"BirthYear\",\"info\":\"1987\"}}}"

enl :: Domain
enl = Enrollment emptyEnrollmentFact

jsonEnrollTest :: B.ByteString
jsonEnrollTest =
    "{\"domain\":\"Enrollment\",\"facts\":{}}"
-- "{\"domain\":\"Enrollment\",\"facts\":{\"plan\":{\"exchange\":\"None\"}}}"

dth :: Domain
dth = Death DeathFacts

jsonDeathTest :: B.ByteString
jsonDeathTest = "{\"domain\":\"Death\",\"facts\":{}}"

src1 :: HC.Source
src1 = HC.Source { table  = "someTable"
                 , row    = Just 1
                 , column = Just "someColumn"
                 , file   = Just "someFile"
                 }

jsonSrc1Test :: B.ByteString
jsonSrc1Test =
  "{\"table\":\"someTable\",\"row\":1,\"file\":\"someFile\",\"column\":\"someColumn\"}"

jsonOtherTest :: B.ByteString
jsonOtherTest = "{\"domain\":\"NotANothing\",\"facts\":{\"code\":{\"code\":\"XYZ\"}}}"

spec :: Spec
spec = do
  it "an Int event is parsed correctly"
    $          decode testInInt
    `shouldBe` Just testOutInt1
  it "lines of Int events are parsed correctly"
    $          parseEventIntLines testInputsInt
    `shouldBe` ([], [testOutInt1, testOutInt2])

  it "a Day event is parsed correctly"
    $          decode testInDay
    `shouldBe` Just testOutDay1
  it "a Day event with missing end day is parsed correctly"
    $          decode testInDay2
    `shouldBe` Just testOutDay1
  it "lines of Day events are parsed correctly"
    $          parseEventDayLines testInputsDay
    `shouldBe` ([], [testOutDay1, testOutDay2])
  it "jsonDemoTest is parsed correctly"
    $          decode jsonDemoTest
    `shouldBe` Just dmo
  it "jsonEnrollTest is parsed correctly"
    $          decode jsonEnrollTest
    `shouldBe` Just enl
  it "jsonDeathTest is parsed correctly"
    $          decode jsonDeathTest
    `shouldBe` Just dth
  it "jsonOtherTest is parsed correctly" $ decode jsonOtherTest `shouldBe` Just
    (UnimplementedDomain ())

  it "jsonSrc1Test is parsed correctly"
    $          decode jsonSrc1Test
    `shouldBe` Just src1

  it "time is parsed correctly"
    $          (decode "{\"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-01\"}}" :: Maybe (EDMInterval Day))
    `shouldBe` Just (EDMInterval (beginerval 1 (fromGregorian 2020 1 1)))
  it "time is parsed correctly"
    $          (decode "{\"time\":{\"begin\":\"2020-01-01\",\"end\":\"2020-01-02\"}}" :: Maybe (EDMInterval Day))
    `shouldBe` Just (EDMInterval (beginerval 2 (fromGregorian 2020 1 1)))
  it "time is parsed correctly"
    $          (decode "{\"time\":{\"begin\":\"2020-01-01\",\"end\":\"2019-12-30\"}}" :: Maybe (EDMInterval Day))
    `shouldBe` Nothing
  it "time is parsed correctly"
    $          (decode "{\"time\":{\"begin\":\"2020-01-01\",\"end\":\"2019-12-31\"}}" :: Maybe (EDMInterval Day))
    `shouldBe` Nothing
