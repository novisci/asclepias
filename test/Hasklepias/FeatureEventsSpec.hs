{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Hasklepias.FeatureEventsSpec (spec) where

import IntervalAlgebra
import Hasklepias.FeatureEvents
import EventData ( Event, event )
import EventData.Context as HC ( context, packConcepts )
import EventData.Context.Domain
import Data.Maybe
import Data.Time.Calendar ( fromGregorian )
import Test.Hspec ( it, shouldBe, Spec )

-- | Toy events for unit tests
evnt1 :: Event Int
evnt1 = event ( beginerval (4 :: Int) (1 :: Int) ) 
        ( HC.context (UnimplementedDomain ()) (packConcepts ["c1", "c2"] ))
evnt2 :: Event Int
evnt2 = event ( beginerval (4 :: Int) (2 :: Int) )
         ( HC.context (UnimplementedDomain ())  (packConcepts ["c3", "c4"] ))
evnts :: [Event Int]
evnts = [evnt1, evnt2]

evntGender :: Event Int
evntGender = event ( beginerval (4 :: Int) (2 :: Int) )
         ( HC.context (Demographics ( DemographicsFacts (DemographicsInfo Gender (Just "F"))))
           (packConcepts [] ))

spec :: Spec
spec = do
    it "find first occurrence of c1" $
      firstConceptOccurrence ["c1"] evnts `shouldBe` Just evnt1
    it "find first occurrence of c3" $
      firstConceptOccurrence ["c3"] evnts `shouldBe` Just evnt2
    it "find first occurrence of c5" $
      firstConceptOccurrence ["c5"] evnts `shouldBe` Nothing

    it "yearFromDay" $
      yearFromDay (fromGregorian 2021 8 18) `shouldBe` 2021
    it "monthFromDay" $
      monthFromDay (fromGregorian 2021 8 18) `shouldBe` 8
    it "dayOfMonthFromDay" $
      dayOfMonthFromDay (fromGregorian 2021 8 18) `shouldBe` 18

    it "viewGenders on empty list" $
      viewGenders [] `shouldBe` []
    it "viewGenders with no demographic events" $
      viewGenders evnts `shouldBe` []
    it "viewGenders with a demographic event" $
      viewGenders [evntGender] `shouldBe` ["F"]