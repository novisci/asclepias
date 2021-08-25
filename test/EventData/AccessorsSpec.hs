{-# LANGUAGE OverloadedStrings #-}
module EventData.AccessorsSpec (spec) where

import IntervalAlgebra
import EventData ( Event, event )
import EventData.Accessors
import EventData.Context as HC ( context, packConcepts )
import Test.Hspec ( shouldBe, it, Spec )
import Data.Maybe (Maybe(Nothing))
import Data.Set(fromList)
import EventData.Context.Domain

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
    it "viewGenders on empty list" $
      viewGenders [] `shouldBe` []
    it "viewGenders with no demographic events" $
      viewGenders evnts `shouldBe` []
    it "viewGenders with a demographic event" $
      viewGenders [evntGender] `shouldBe` ["F"]
