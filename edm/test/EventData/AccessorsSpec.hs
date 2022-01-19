{-# LANGUAGE OverloadedStrings #-}
module EventData.AccessorsSpec
  ( spec
  ) where

import           Data.Aeson                     ( decode, eitherDecode )
import           Data.Functor.Contravariant
import           Data.Maybe                     ( Maybe(Nothing) )
import           Data.Set                       ( fromList )
import           EventData                      ( Event
                                                , event
                                                )
import           EventData.Accessors
import           EventData.Context             as HC
                                                ( context
                                                , packConcepts
                                                )
import           EventData.Context.Domain
import           EventData.Predicates
import           IntervalAlgebra
import qualified Data.ByteString.Lazy          as B
import           Test.Hspec                     ( Spec
                                                , it
                                                , shouldBe
                                                )

-- | Toy events for unit tests
evnt1 :: Event Int
evnt1 = event
  (beginerval (4 :: Int) (1 :: Int))
  (HC.context (UnimplementedDomain ()) (packConcepts ["c1", "c2"]) Nothing)
evnt2 :: Event Int
evnt2 = event
  (beginerval (4 :: Int) (2 :: Int))
  (HC.context (UnimplementedDomain ()) (packConcepts ["c3", "c4"]) Nothing)
evnts :: [Event Int]
evnts = [evnt1, evnt2]

demoYear :: Domain
demoYear =
  Demographics $ DemographicsFacts (DemographicsInfo BirthYear (Just "1987"))

evntGender :: Event Int
evntGender = event
  (beginerval (4 :: Int) (2 :: Int))
  (HC.context
    (Demographics (DemographicsFacts (DemographicsInfo Gender (Just "F"))))
    (packConcepts [])
    Nothing
  )

evntYear :: Event Int
evntYear = event (beginerval (4 :: Int) (2 :: Int))
                 (HC.context demoYear (packConcepts []) Nothing)

enrollEvent :: B.ByteString
enrollEvent = 
  "[\"abc\",0,1,\"Enrollment\",[]\
  \,{\"patient_id\":\"abc\"\
  \,\"time\":{\"begin\":0,\"end\":1}\
  \,\"domain\":\"Enrollment\"\
  \,\"facts\":{\"plan\":{\"benefit\":\"PPO\",\"exchange\":\"None\"}}}]"


spec :: Spec
spec = do
  it "viewGenders on empty list" $ viewGenders [] `shouldBe` []
  it "viewGenders with no demographic events" $ viewGenders evnts `shouldBe` []
  it "viewGenders with a demographic event"
    $          viewGenders [evntGender]
    `shouldBe` ["F"]

  it "previewBirthYear on demographic domain"
    $          previewBirthYear demoYear
    `shouldBe` Just 1987
  it "isBirthYearEvent on demographic domain with BirthYear"
    $          getPredicate isBirthYearEvent evntYear
    `shouldBe` True
  it "viewBirthYears on demographic event"
    $          viewBirthYears [evntYear]
    `shouldBe` [1987]
  it "viewBenefits on enrollment event"
    $         fmap viewBenefits (sequenceA [decode enrollEvent :: Maybe (Event Int)])
    `shouldBe` Just ["PPO"]
