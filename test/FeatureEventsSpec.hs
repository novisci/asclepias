{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module FeatureEventsSpec (spec) where

import IntervalAlgebra
import FeatureEvents ( firstConceptOccurrence )
import EventData ( Event, event )
import EventData.Context as HC ( context, packConcepts )
import Data.Maybe
import Test.Hspec ( it, shouldBe, Spec )

-- | Toy events for unit tests
evnt1 :: Event Int
evnt1 = event ( beginerval (4 :: Int) (1 :: Int) ) 
        ( HC.context Nothing (packConcepts ["c1", "c2"] ))
evnt2 :: Event Int
evnt2 = event ( beginerval (4 :: Int) (2 :: Int) )
         ( HC.context Nothing (packConcepts ["c3", "c4"] ))
evnts :: [Event Int]
evnts = [evnt1, evnt2]

spec :: Spec
spec = do
    it "find first occurrence of c1" $
      firstConceptOccurrence ["c1"] evnts `shouldBe` Just evnt1
    it "find first occurrence of c3" $
      firstConceptOccurrence ["c3"] evnts `shouldBe` Just evnt2
    it "find first occurrence of c5" $
      firstConceptOccurrence ["c5"] evnts `shouldBe` Nothing