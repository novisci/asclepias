{-# LANGUAGE OverloadedStrings #-}
module Hasklepias.Types.EventSpec (spec) where

import IntervalAlgebra
import Hasklepias.Functions
import Hasklepias.Types.Event
import Hasklepias.Types.Context as HC
import Test.Hspec ( it, shouldBe, Spec )

evnt1 :: Event Int
evnt1 = event ( beginerval 4 (1 :: Int))
              ( HC.context $ packConcepts ["c1", "c2"] )
evnt2 :: Event Int
evnt2 = event ( beginerval 4 (2 :: Int)  )
              ( HC.context $ packConcepts ["c3", "c4"] )
evnts :: [Event Int]
evnts = [evnt1, evnt2]

containmentInt :: Interval Int
containmentInt = beginerval 10 (0 :: Int) 

noncontainmentInt :: Interval Int
noncontainmentInt = beginerval 6 (4 :: Int) 

anotherInt :: Interval Int
anotherInt = beginerval 5 (15 :: Int) 

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
      filterEvents (`hasConcept` "c1") evnts `shouldBe` [evnt1]
    it "filterEvents for evnt2" $
      filterEvents (`hasConcept` "c3") evnts `shouldBe` [evnt2]
    it "filterEvents to no events" $
      filterEvents (`hasConcept` "c5") evnts `shouldBe` []

    it "lift2IntervalPredicate meets" $
      lift2IntervalPredicate meets evnt1 evnt2 `shouldBe` False
    it "lift2IntervalPredicate overlaps" $
      lift2IntervalPredicate overlaps evnt1 evnt2 `shouldBe` True
    it "lift2IntervalPredicate overlaps" $
      lift2IntervalPredicate overlappedBy evnt2 evnt1 `shouldBe` True

    it "filterEvents by interval containment" $
      filterEvents (liftIntervalPredicate contains containmentInt) evnts `shouldBe` evnts
    it "filterEvents by interval containment" $
      filterEvents (liftIntervalPredicate contains noncontainmentInt) evnts `shouldBe` []

