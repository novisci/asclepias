{-# LANGUAGE OverloadedStrings #-}
module Hasklepias.FeaturesSpec (spec) where

import IntervalAlgebra
import Hasklepias.Features
import Hasklepias.Types.Event
import Hasklepias.Types.Event.Examples
import Hasklepias.Types.Context as HC
import Data.Time as DT
import Test.Hspec

evnt1 = event ( unsafeInterval (1 :: Int) (5 :: Int) ) ( HC.context ["c1", "c2"] ) 
evnt2 = event ( unsafeInterval (2 :: Int) (6 :: Int) ) ( HC.context ["c3", "c4"] ) 
evnts = [evnt1, evnt2]


indexExample :: (IntervalAlgebraic a) =>
                Events a -> Feature (Interval a)
indexExample es = 
    case firstOccurrenceOfConcept "wasBitByOrca" es of 
        Nothing -> Deficient  "No occurrence of Orca Bite"
        Just x  -> Sufficient "index" (intrvl x)

baselineInterval :: (Num a, IntervalAlgebraic a) =>
                    Feature (Interval a) -> Maybe (Interval a)
baselineInterval (Deficient  _ )  = Nothing
baselineInterval (Sufficient _ x) = Just (unsafeInterval (begin x - 90) (begin x))

makeBaselineFilter :: IntervalAlgebraic a => 
                     Maybe (Interval a) 
                  -> Maybe (Event a -> Bool)
makeBaselineFilter Nothing  = Nothing
makeBaselineFilter (Just x) = Just (\e -> (intrvl e) `in'` x)


hasDuckHistory :: (IntervalAlgebraic a) =>
                     Maybe (Event a -> Bool)
                  -> Events a 
                  -> Feature Bool
hasDuckHistory Nothing _  = Deficient "No baseline"
hasDuckHistory (Just x) es = 
      Sufficient "History with Ducks" 
        (not $ null $
          (filterEvents x) $
          (filterEvents (\e -> 
            e `hasConcepts` ["wasBitByDuck", "wasStruckByDuck"]))
          es)

hasMacawHistory :: (Num a, IntervalAlgebraic a) =>
                     Maybe (Event a -> Bool)
                  -> Events a 
                  -> Feature Bool
hasMacawHistory Nothing _  = Deficient "No baseline"
hasMacawHistory (Just x) es = 
      Sufficient "History with Macaw" 
        (not $ null $
          (filterEvents x) $
          (filterEvents (\e ->
             e `hasConcepts` ["wasBitByMacaw", "wasStruckByMacaw"])) 
          es)


-- baselineInterval1 = (baselineInterval.indexExample) exampleEvents1
baselineFilter1 = (makeBaselineFilter.baselineInterval.indexExample) exampleEvents1
baselineFilter2 = (makeBaselineFilter.baselineInterval.indexExample) exampleEvents2
{-
Define enrolled as the indicator of whether all of the gaps between the union of 
all periods (+ allowableGap) that are overlapped by the lookbackPeriod are less
than maxGap
-}
{-
enrolled allowableGap indexPoint =
   all (< allowableGap) . 
   durations . 
   periodGaps . 
   baselineFilter indexPoint . 
   collapsePeriods
-}


spec :: Spec 
spec = do 
    it "find first occurrence of c1" $ 
      (firstOccurrenceOfConcept "c1" evnts) `shouldBe` (Just evnt1)
    it "find first occurrence of c3" $ 
      (firstOccurrenceOfConcept "c3" evnts) `shouldBe` (Just evnt2)
    it "find first occurrence of c5" $ 
      (firstOccurrenceOfConcept "c5" evnts) `shouldBe` Nothing

    it "indexExample of exampleEvents1" $ 
      (indexExample exampleEvents1) `shouldBe`
      (Sufficient "index" (unsafeInterval (89 :: Int) (90 ::Int)))
    it "baselineInterval from exampleEvents1" $ 
      (baselineInterval (indexExample exampleEvents1)) `shouldBe` 
      (Just (unsafeInterval (-1 :: Int) (89 :: Int)))
    it "hasDuckHistory from exampleEvents1" $ 
      (hasDuckHistory baselineFilter1 exampleEvents1) `shouldBe` 
      (Sufficient "History with Ducks" True)
    it "hasMacawHistory from exampleEvents1" $ 
      (hasMacawHistory baselineFilter1 exampleEvents1) `shouldBe` 
      (Sufficient "History with Macaw" False)

    it "indexExample of exampleEvents2" $ 
      (indexExample exampleEvents2) `shouldBe`
      (Deficient  "No occurrence of Orca Bite")
    it "hasDuckHistory from exampleEvents1" $ 
      (hasDuckHistory baselineFilter2 exampleEvents2) `shouldBe` 
      (Deficient  "No baseline")
    it "hasMacawHistory from exampleEvents1" $ 
      (hasMacawHistory baselineFilter2 exampleEvents2) `shouldBe` 
      (Deficient  "No baseline")