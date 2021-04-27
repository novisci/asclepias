{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Hasklepias.FeaturesSpec (spec) where

import IntervalAlgebra
import IntervalAlgebra.IntervalUtilities
import Hasklepias.Features
import Hasklepias.Types.Event
import Hasklepias.Types.Event.Examples
import Hasklepias.Types.Context as HC
import Data.Time as DT
import Test.Hspec
import Data.Maybe
import Control.Monad
import Control.Arrow

-- | Toy events for unit tests
evnt1 :: Event Int
evnt1 = event ( beginerval (4 :: Int) (1 :: Int) ) ( HC.context ["c1", "c2"] )
evnt2 :: Event Int
evnt2 = event ( beginerval (4 :: Int) (2 :: Int) ) ( HC.context ["c3", "c4"] )
evnts :: [Event Int]
evnts = [evnt1, evnt2]


{- Helper functions used below -}

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast x  = Just $ last x

hasAny :: [a] -> Bool
hasAny = not.null

makeConceptsFilter :: (IntervalAlgebraic a) =>
       [Concept]
    -> Events a
    -> Events a
makeConceptsFilter cpts = filterEvents (`hasConcepts` cpts)

nthOccurenceOfConcepts :: (IntervalAlgebraic a) =>
       (Events a -> Maybe (Event a))
    -> [Concept]
    -> Events a
    -> Maybe (Event a)
nthOccurenceOfConcepts f c = f.makeConceptsFilter c

firstOccurrenceOfConcepts :: (IntervalAlgebraic a) =>
      [Concept]
    -> Events a
    -> Maybe (Event a)
firstOccurrenceOfConcepts = nthOccurenceOfConcepts safeHead

makeIntervalFilter :: (IntervalAlgebraic a) =>
       ComparativePredicateOf (Interval a)
    -> Interval a
    -> Events a
    -> Events a
makeIntervalFilter f i = filterEvents $ liftIntervalPredicate f i

inFilter :: (IntervalAlgebraic a) => Interval a -> Events a -> Events a
inFilter = makeIntervalFilter in'

-- | Assumes x <= y
clipInterval :: (IntervalAlgebraic a, IntervalSizeable a b)=>
  Interval a -> Interval a -> Maybe (Interval a)
clipInterval x y
  | overlappedBy x y = Just $ beginerval (diff (end y) (begin x)) (begin x)
  | overlaps     x y = Just $ enderval   (diff (end x) (begin y)) (end x)
  | disjoint x y     = Nothing
  | otherwise        = Just x

emptyIfNone :: (IntervalFilterable [] a)=>
     (Interval a ->  Bool)
  -> [Interval a]
  -> [Interval a]
emptyIfNone f x = if (not.any f) x then x else []

gapsWithin :: (IntervalSizeable a b, IntervalCombinable a, IntervalFilterable [] a)=>
      Interval a
  -> [Interval a]
  -> [Interval a]
gapsWithin i x = gaps $ enderval 0 (begin i) : 
                        mapMaybe (clipInterval i) (filterNotDisjoint i x) ++
                        [beginerval 0 (end i)]

makeHasAnyEventFeature :: (IntervalAlgebraic a) =>
                     String
                  -> Feature (Events a -> Events a)
                  -> (Events a -> Feature Bool)
makeHasAnyEventFeature sstring feat es =
  case feat of
   (Deficient x )   -> Deficient  x
   (Sufficient _ f) -> Sufficient sstring ((hasAny.f) es)



-- (>>>=) :: Monad m => m (a -> b) -> m (b -> c) -> m (a -> c)
-- (>>>=) = liftM2 (>>>)

{-
 Functions for building an example cohort
-}
index:: (IntervalAlgebraic a) => Events a -> Feature (Interval a)
index es =
    case firstOccurrenceOfConcepts ["wasBitByOrca"] es of
        Nothing -> Deficient  "No occurrence of Orca Bite"
        Just x  -> Sufficient "index" (intrvl x)

baseline :: (IntervalAlgebraic a, IntervalSizeable a b) =>
            Feature (Interval a)
         -> Feature (Events a -> Events a)
baseline (Deficient  x)   = Deficient x
baseline (Sufficient _ x) = Sufficient "baseline" $ inFilter $ enderval 90 (begin x)

-- baseline' :: (IntervalAlgebraic a, IntervalSizeable a b) => 
--               Events a -> Feature (Events a)
-- baseline' i = inFilter $ enderval 90 (begin x) 

ducks :: (IntervalAlgebraic a, IntervalSizeable a b) =>
            Feature (Events a -> Events a)
         -> Feature (Events a -> Events a)
ducks (Deficient  x )  = Deficient x
ducks (Sufficient _ x) = Sufficient "ducks" $
                         makeConceptsFilter ["wasBitByDuck", "wasStruckByDuck"]

macaws :: (IntervalAlgebraic a, IntervalSizeable a b) =>
            Feature (Events a -> Events a)
         -> Feature (Events a -> Events a)
macaws (Deficient  x )  = Deficient x
macaws (Sufficient _ x) = Sufficient "macaws" $
                          makeConceptsFilter ["wasBitByMacaw", "wasStruckByMacaw"]


hasDuckHistory :: (IntervalAlgebraic a, IntervalSizeable a b) =>
      Events a -> Feature Bool
hasDuckHistory x = makeHasAnyEventFeature "History with Ducks"
                   ((ducks.baseline.index) x) x

hasMacawHistory :: (IntervalAlgebraic a, IntervalSizeable a b) =>
      Events a -> Feature Bool
hasMacawHistory x = makeHasAnyEventFeature "History with Macaw"
                   ((macaws.baseline.index) x) x


{-
Define enrolled as the indicator of whether all of the gaps between the union of 
all periods (+ allowableGap) that are overlapped by the lookbackPeriod are less
than maxGap
-}


enrolled' :: (IntervalSizeable a b, IntervalCombinable a) =>
     b -> Events a -> Bool
enrolled' allowableGap =
       all(< allowableGap)
      .durations
      .gaps
      -- need to apply baseline filter here
      .combineIntervals
      .intervals
      .makeConceptsFilter ["enrollment"]

-- enrolled :: (IntervalSizeable a b, IntervalCombinable a) =>
--     b -> Feature (t -> Events a) -> t -> Feature (Events a)
-- enrolled allowableGap baselineFeature es = 
--   case baselineFeature of
--     (Deficient x)    -> Deficient x
--     (Sufficient _ f) -> Sufficient "Continuous Enrollment" (enrolled' allowableGap (f es))

-- combineIntervals.
-- getIntervals

-- | n of x or m of y
atleastNofX :: (IntervalAlgebraic a) => Int -> [Concept] -> Events a -> Bool
atleastNofX n x es = length (makeConceptsFilter x es) >= n

twoXOrOneY :: (IntervalAlgebraic a) => [Concept] -> [Concept] -> Events a -> Bool
twoXOrOneY x y es = atleastNofX 2 x es ||
                    atleastNofX 1 y es

twoMinorOrOneMajor :: (IntervalAlgebraic a) => Events a -> Bool
twoMinorOrOneMajor = twoXOrOneY ["hadMinorSurgery"] ["hadMajorSurgery"]

-- | Time to end of baseline to end of most recent Antibiotics
--   with 5 day grace period
timeSinceLastAntibiotics :: (IntervalAlgebraic a, IntervalCombinable a, IntervalSizeable a b,
                            IntervalFilterable [] a) =>
      Interval a -> Events a -> b
timeSinceLastAntibiotics blinterval =
   last                                   -- want the last one
  .map (max 0 . diff (end blinterval) . end) -- distances between end of baseline and antibiotic intervals
  .filterNotDisjoint blinterval           -- filter to intervals not disjoint from baseline interval
  .combineIntervals                       -- combine overlapping intervals
  .map (expandr 5)                        -- allow grace period
  .intervals                              -- extract intervals
  .makeConceptsFilter ["tookAntibiotics"]  -- filter to only antibiotics events


-- | Count of hospital events in a interval and duration of the last one
countOfHospitalEvents :: (IntervalCombinable a, IntervalSizeable a b,
                          IntervalFilterable [] a) =>
     Interval a
  -> Events a 
  -> (Int, Maybe b)
countOfHospitalEvents i =
   (\x -> (length x, duration <$> safeLast x))
  .filterNotDisjoint i                    -- filter to intervals not disjoint from interval
  .combineIntervals                       -- combine overlapping intervals
  .intervals                              -- extract intervals
  .makeConceptsFilter ["wasHospitalized"]  -- filter to only antibiotics events

so :: (IntervalAlgebraic a)=> ComparativePredicateOf (Interval a)
so = composeRelations [starts, overlaps]

-- | time of distcontinuation of antibiotics
--   and time from start of follow up
--   This needs to be generalized as Nothing could either indicate they didn't 
--   discontinue or that they simply got no antibiotics records.
discontinuation :: (IntervalSizeable a b, IntervalCombinable a, IntervalFilterable [] a) =>
     Interval a
  -> Events a
  -> Maybe (a, b)
discontinuation i =
   fmap (\x -> (begin x, diff (begin x) (begin i))) -- we want the beginning of this interval 
  .safeHead                  -- if there are any gaps the first one is the first discontinuation
  .gapsWithin i              -- find gaps to intervals clipped to i
  .emptyIfNone (so i)        -- if none of the intervals start or overlap 
                             -- the followup, then never started antibiotics
  .combineIntervals          -- combine overlapping intervals
  .map (expandr 5)           -- allow grace period
  .intervals                 -- extract intervals
  .makeConceptsFilter        -- filter to only antibiotics events
    ["tookAntibiotics"] 

-- | Within baseline interval, either:
--     * >=2 intervals of C with a gap of no more than X 
--       between at least 2 of these intervals 
--     * 1 interval of C at duration of at least Y


spec :: Spec
spec = do
    it "find first occurrence of c1" $
      firstOccurrenceOfConcepts ["c1"] evnts `shouldBe` Just evnt1
    it "find first occurrence of c3" $
      firstOccurrenceOfConcepts ["c3"] evnts `shouldBe` Just evnt2
    it "find first occurrence of c5" $
      firstOccurrenceOfConcepts ["c5"] evnts `shouldBe` Nothing


    it "clipInterval" $
      clipInterval (unsafeInterval (0 :: Int) (10 ::Int)) (unsafeInterval (5 :: Int) (15 ::Int)) `shouldBe`
      Just (unsafeInterval (5 :: Int) (10 ::Int))
    it "clipInterval" $
      clipInterval (unsafeInterval (0 :: Int) (10 ::Int)) (unsafeInterval (-1 :: Int) (5 ::Int)) `shouldBe`
      Just (unsafeInterval (0 :: Int) (5 ::Int))


    it "index of exampleEvents1" $
      index exampleEvents1 `shouldBe`
      Sufficient "index" (unsafeInterval (89 :: Int) (90 ::Int))
    -- it "baseline from exampleEvents1" $
    --   (baseline.index) exampleEvents1 `shouldBe`
    --   Sufficient "baseline" [evnt1]
    it "hasDuckHistory from exampleEvents1" $
      hasDuckHistory exampleEvents1 `shouldBe`
      Sufficient "History with Ducks" True
    it "hasMacawHistory from exampleEvents1" $
      hasMacawHistory exampleEvents1 `shouldBe`
      Sufficient "History with Macaw" False
    it "enrolled' from exampleEvents1 with gap of 8" $
      enrolled' 8 exampleEvents1 `shouldBe` False
    it "enrolled' from exampleEvents1 with gap of 9" $
      enrolled' 9 exampleEvents1 `shouldBe` True
    -- it "enrolled from exampleEvents1 with gap of 8" $
    --   enrolled 8 ((baseline.index) exampleEvents1) exampleEvents1 `shouldBe`
    --   Sufficient "Continuous Enrollment" [evnt1]
    -- it "enrolled from exampleEvents1 with gap of 1" $
    --   enrolled 1 ((baseline.index) exampleEvents1) exampleEvents1 `shouldBe`
    --   Sufficient "Continuous Enrollment" [evnt1]
    it "" $
      timeSinceLastAntibiotics (unsafeInterval (-1 :: Int) (89 ::Int)) exampleEvents1 `shouldBe`
      (11::Int)
    it "" $
      timeSinceLastAntibiotics (unsafeInterval (-1 :: Int) (60 ::Int)) exampleEvents1 `shouldBe`
      (0::Int)
    it "" $
      countOfHospitalEvents (unsafeInterval (-1 :: Int) (60 ::Int)) exampleEvents1 `shouldBe`
      (1::Int, Just (8::Int))

    -- it "" $
    --   discontinuation (unsafeInterval (40 :: Int) (90 ::Int)) exampleEvents1 `shouldBe`
    --   Nothing
      -- Just (60::Int)
    it "" $
      discontinuation (unsafeInterval (60 :: Int) (90 ::Int)) exampleEvents1 `shouldBe`
      Just (78::Int, 18)
    it "" $
      discontinuation (unsafeInterval (70 :: Int) (90 ::Int)) exampleEvents1 `shouldBe`
      -- Nothing
      Just (78::Int, 8)
    it "" $
      discontinuation (unsafeInterval (50 :: Int) (90 ::Int)) exampleEvents1 `shouldBe`
      Nothing
    it "" $
      discontinuation (unsafeInterval (60 :: Int) (75 ::Int)) exampleEvents1 `shouldBe`
      Nothing
      -- Just (78::Int)

    it "index of exampleEvents2" $
      index exampleEvents2 `shouldBe`
      Deficient  "No occurrence of Orca Bite"
    it "hasDuckHistory from exampleEvents2" $
      hasDuckHistory exampleEvents2 `shouldBe`
      Deficient  "No occurrence of Orca Bite"
    it "hasMacawHistory from exampleEvents2" $
      hasMacawHistory exampleEvents2 `shouldBe`
      Deficient  "No occurrence of Orca Bite"