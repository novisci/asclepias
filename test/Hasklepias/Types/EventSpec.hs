{-# LANGUAGE OverloadedStrings #-}
module Hasklepias.Types.EventSpec (spec) where

import IntervalAlgebra
import Hasklepias.Functions
import Hasklepias.Types.Event
import Hasklepias.Types.Context as HC
import Test.Hspec

evnt1 = event ( unsafeInterval (1 :: Int) (5 :: Int) ) ( HC.context ["c1", "c2"] ) 
evnt2 = event ( unsafeInterval (2 :: Int) (6 :: Int) ) ( HC.context ["c3", "c4"] ) 
evnts = [evnt1, evnt2]
containmentInt = unsafeInterval (0 :: Int) (10 :: Int)
noncontainmentInt = unsafeInterval (4 :: Int) (10 :: Int)
anotherInt = unsafeInterval (15 :: Int) (20 :: Int)

spec :: Spec 
spec = do 
    it "hasConcept returns True when concept is in context" $ 
      (evnt1 `hasConcept` "c1") `shouldBe` True
    it "hasConcept returns False when concept is not in context" $ 
      (evnt1 `hasConcept` "c3") `shouldBe` False
    it "hasConcepts returns True when at at least one concept is in context" $ 
      (evnt1 `hasConcepts` ["c3", "c1"]) `shouldBe` True
    it "hasConcepts returns False when no concept is in context" $ 
      (evnt1 `hasConcepts` ["c3", "c4"]) `shouldBe` False

    it "filterEvents for evnt1" $ 
      (filterEvents (`hasConcept` "c1") evnts) `shouldBe` [evnt1]
    it "filterEvents for evnt2" $ 
      (filterEvents (`hasConcept` "c3") evnts) `shouldBe` [evnt2]
    it "filterEvents to no events" $ 
      (filterEvents (`hasConcept` "c5") evnts) `shouldBe` []

    it "lift2IntervalPredicate meets" $ 
      (lift2IntervalPredicate meets evnt1 evnt2) `shouldBe` False
    it "lift2IntervalPredicate overlaps" $ 
      (lift2IntervalPredicate overlaps evnt1 evnt2) `shouldBe` True
    it "lift2IntervalPredicate overlaps" $ 
      (lift2IntervalPredicate overlappedBy evnt2 evnt1) `shouldBe` True

    it "filterEvents by interval containment" $ 
      (filterEvents (liftIntervalPredicate contains containmentInt) evnts) `shouldBe` evnts
    it "filterEvents by interval containment" $ 
      (filterEvents (liftIntervalPredicate contains noncontainmentInt) evnts) `shouldBe` []

