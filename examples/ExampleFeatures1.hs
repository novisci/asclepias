{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : ExampleFeatures1
Description : Demostrates how to define features using Hasklepias
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

module ExampleFeatures1(
    exampleFeatures1Spec
  , dropEventFilter
  , index
  , baseline
  , enrolled
  , enrolled'
  , pack
) where

import Hasklepias
import ExampleEvents
import Test.Hspec
import Data.Text(pack)
import Data.Maybe ( fromMaybe )
import Control.Monad

{- Helper functions used below -}

dropEventFilter :: (IntervalAlgebraic a)=>
     (Events a -> Events a)
  -> [Interval a]
  -> [Interval a]
dropEventFilter f l = (intervals . f) (fmap (`event` emptyContext) l)

{-
Index is defined as the first occurrence of an Orca bite.
-}
index:: (IntervalAlgebraic a) =>
     Events a
  -> Feature (Interval a)
index es =
    case firstConceptOccurrence ["wasBitByOrca"] es of
        Nothing -> Left $ Other "No occurrence of Orca bite"
        Just x  -> Right (intrvl x)

{-  
The baseline interval is the interval (b - 60, b), where b is the begin of 
index. Here, baseline is defined as function that takes a filtration function
as an argument, so that the baseline feature can be used to filter events
based on different predicate functions.
-}
baseline :: (IntervalAlgebraic a, IntervalSizeable a b) =>
     Feature (Interval a) -- ^ pass the result of index to get a baseline filter
  -> Feature (Interval a)
baseline = fmap (enderval 60 . begin)

bline :: (IntervalAlgebraic a, IntervalSizeable a b) =>
     Events a
  -> Feature (Interval a)
bline = baseline.index

flwup :: (IntervalAlgebraic a, IntervalSizeable a b) =>
     Events a
  -> Feature (Interval a)
flwup = fmap (beginerval 30 . begin) . index
{-
Define enrolled as the indicator of whether all of the gaps between the union of 
all enrollment intervals (+ allowableGap) 
-}
enrolled :: (IntervalSizeable a b, IntervalCombinable a) =>
      Feature (Interval a)
   -> Events a
   -> Feature Bool
enrolled (Left  _) _ = Left Excluded
enrolled (Right i) l = Right $ enrolled' 8 i l

enrolled' :: (IntervalSizeable a b, IntervalCombinable a) =>
      b  -- ^ allowable gap between enrollment intervals
   -> Interval a -- ^ baseline interval
   -> Events a
   -> Bool
enrolled' allowableGap i =
        maybe False (all (< allowableGap) . durations)
      . gapsWithin i
      . combineIntervals
      . intervals
      . makeConceptsFilter ["enrollment"]

{-
Define features that identify whether a subject as bit/struck by a duck and
bit/struck by a macaw.
-}
makeHasAnyHistoryFeature :: (IntervalAlgebraic a) =>
       [Concept]
    -> Feature (Interval a)
    -> Events a
    -> Feature (Bool, Maybe (Interval a))
makeHasAnyHistoryFeature cnpts feat es =
  case feat of
   (Left x ) -> Left Excluded
   (Right i) -> Right (isNotEmpty candidates, safeLast $ intervals candidates)
      where candidates = makeEventFilter overContainment i (`hasConcepts` cnpts) es

hasDuckHistory :: (IntervalAlgebraic a, IntervalSizeable a b) =>
       Events a
    -> Feature (Bool, Maybe (Interval a))
hasDuckHistory x = makeHasAnyHistoryFeature
   ["wasBitByDuck", "wasStruckByDuck"]
   (bline x ) x

hasMacawHistory :: (IntervalAlgebraic a, IntervalSizeable a b) =>
      Events a
    -> Feature (Bool, Maybe (Interval a))
hasMacawHistory x = makeHasAnyHistoryFeature
   ["wasBitByMacaw", "wasStruckByMacaw"]
   (bline x) x

{-
Define an event that identifies whether the subject has two minor or one major
surgery 
-}

twoMinorOrOneMajor :: (IntervalAlgebraic a) =>
     Feature (Interval a)
  -> Events a
  -> Feature Bool
twoMinorOrOneMajor feat l =
 case feat of
   Left x  -> Left Excluded
   Right i -> Right $ twoXOrOneY ["hadMinorSurgery"] ["hadMajorSurgery"] (overFilter i l)


-- | Time from end of baseline to end of most recent Antibiotics
--   with 5 day grace period
timeSinceLastAntibiotics :: (IntervalAlgebraic a, IntervalCombinable a, IntervalSizeable a b) =>
         Feature (Interval a)
      -> Events a
      -> Feature (Maybe b)
timeSinceLastAntibiotics (Left _) _   = Left Excluded
timeSinceLastAntibiotics (Right i) es = Right $ timeSinceLastAntibiotics' i es


timeSinceLastAntibiotics' :: (IntervalAlgebraic a, IntervalCombinable a, IntervalSizeable a b) =>
      Interval a -> Events a -> Maybe b
timeSinceLastAntibiotics' blinterval =
   safeLast                                   -- want the last one
  .map (max 0 . diff (end blinterval) . end) -- distances between end of baseline and antibiotic intervals
  .filterNotDisjoint blinterval           -- filter to intervals not disjoint from baseline interval
  .combineIntervals                       -- combine overlapping intervals
  .map (expandr 5)                        -- allow grace period
  .intervals                              -- extract intervals
  .makeConceptsFilter ["tookAntibiotics"]  -- filter to only antibiotics events


-- | Count of hospital events in a interval and duration of the last one
countOfHospitalEvents :: (IntervalCombinable a, IntervalSizeable a b) =>
     Feature (Interval a)
  -> Events a
  -> Feature (Int, Maybe b)
countOfHospitalEvents (Left _) _   = Left Excluded
countOfHospitalEvents (Right i) es = Right $
            ((\x -> (length x, duration <$> safeLast x))
            .filterNotDisjoint i                    -- filter to intervals not disjoint from interval
            .combineIntervals                       -- combine overlapping intervals
            .intervals                              -- extract intervals
            .makeConceptsFilter ["wasHospitalized"])  -- filter to only antibiotics events
            es

so :: (IntervalAlgebraic a)=> ComparativePredicateOf (Interval a)
so = unionPredicates [startedBy, overlappedBy]

-- | time of distcontinuation of antibiotics
--   and time from start of follow up
--   This needs to be generalized as Nothing could either indicate they didn't 
--   discontinue or that they simply got no antibiotics records.
discontinuation :: (IntervalSizeable a b, IntervalCombinable a) =>
     Feature (Interval a)
  -> Events a
  -> Feature (Maybe (a, b))
--   -> Feature (Maybe (Interval a))
discontinuation (Left _) _    = Left Excluded
discontinuation (Right i) es  = Right $
      (\x -> Just (begin x           -- we want the begin of this interval 
                  , diff (begin x) (begin i)))
      =<< safeHead                   -- if there are any gaps the first one is the first discontinuation
      =<< gapsWithin i               -- find gaps to intervals clipped to i
      =<< (nothingIfNone (so i)      -- if none of the intervals start or overlap 
                                     -- the followup, then never started antibiotics
         . combineIntervals          -- combine overlapping intervals
         . map (expandr 5)           -- allow grace period
         . intervals                 -- extract intervals
         . makeConceptsFilter        -- filter to only antibiotics events
            ["tookAntibiotics"]) es


getUnitFeatures ::
      Events Int
  -> (Feature (Interval Int)
     , Feature Bool
     , Feature (Bool, Maybe (Interval Int))
     , Feature (Bool, Maybe (Interval Int))
     , Feature Bool
     , Feature (Maybe Int)
     , Feature (Int, Maybe Int)
     , Feature (Maybe (Int, Int))
     )
getUnitFeatures x = (
    index x
  , enrolled (bline x) x
  , hasDuckHistory x
  , hasMacawHistory x
  , twoMinorOrOneMajor (bline x) x
  , timeSinceLastAntibiotics (bline x) x
  , countOfHospitalEvents (bline x) x
  , discontinuation (flwup x) x
  )

exampleFeatures1Spec :: Spec
exampleFeatures1Spec = do

    it "getUnitFeatures from exampleEvents1" $
      getUnitFeatures exampleEvents1 `shouldBe`
      ( Right (unsafeInterval (60 :: Int) (61 ::Int))
      , Right True
      , Right (True, Just $ unsafeInterval (51 :: Int) (52 :: Int))
      , Right (False, Nothing)
      , Right True
      , Right $ Just 4
      , Right (1, Just 8)
      , Right $ Just (78, 18)
      )

    it "getUnitFeatures from exampleEvents2" $
      getUnitFeatures exampleEvents2 `shouldBe`
      ( Left (Other "No occurrence of Orca bite")
      , Left Excluded
      , Left Excluded
      , Left Excluded
      , Left Excluded
      , Left Excluded
      , Left Excluded
      , Left Excluded
      )
