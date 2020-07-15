{-# LANGUAGE OverloadedStrings #-}
module Hasklepias.FeaturesSpec (spec) where

import IntervalAlgebra
import Hasklepias.Features
import Hasklepias.Types.Event
import Hasklepias.Types.Context as HC
import Data.Time as DT
import Test.Hspec

evnt1 = event ( unsafeInterval (1 :: Int) (5 :: Int) ) ( HC.context ["c1", "c2"] ) 
evnt2 = event ( unsafeInterval (2 :: Int) (6 :: Int) ) ( HC.context ["c3", "c4"] ) 
evnts = [evnt1, evnt2]


indexExample :: (IntervalAlgebraic a) =>
                Events a -> Feature (Interval a)
indexExample es = 
    case firstOccurrenceOfConcept "c1" es of 
        Nothing -> Deficient  "No occurrence of c1"
        Just x  -> Sufficient "index" (intrvl x)

baselineInterval :: (Num a, IntervalAlgebraic a) =>
                    Feature (Interval a) -> Maybe (Interval a)
baselineInterval (Deficient  _ )  = Nothing
baselineInterval (Sufficient _ x) = Just (unsafeInterval (begin x - 365) (begin x))

spec :: Spec 
spec = do 
    it "find first occurrence of c1" $ 
      (firstOccurrenceOfConcept "c1" evnts) `shouldBe` (Just evnt1)
    it "find first occurrence of c3" $ 
      (firstOccurrenceOfConcept "c3" evnts) `shouldBe` (Just evnt2)
    it "find first occurrence of c5" $ 
      (firstOccurrenceOfConcept "c5" evnts) `shouldBe` Nothing

    it "indexExample of c1" $ 
      (indexExample evnts) `shouldBe` (Sufficient "index" (intrvl evnt1))

    it "baselineInterval from indexExample" $ 
      (baselineInterval (indexExample evnts)) `shouldBe` 
      (Just (unsafeInterval (-364 :: Int) 1))
