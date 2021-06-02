{-|
Module      : ExampleFeatures1
Description : Demostrates how to define features using Hasklepias
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ExampleFeatures1(
    exampleFeatures1Spec
) where

import Hasklepias
import ExampleEvents
import Test.Hspec
import Control.Monad

{-
Index is defined as the first occurrence of an Orca bite.
-}
indexDef :: (Ord a) => FeatureDefinition
  (FeatureData (Events a))
  (Interval a)
indexDef = defineM (\events ->
  case firstConceptOccurrence ["wasBitByOrca"]  events of
        Nothing -> featureDataL (Other "No occurrence of Orca bite")
        Just x  -> pure (getInterval x))

-- indexSpec :: (Ord a) => FeatureSpec Text (*) (Events a) (Interval a)
-- indexSpec = makeFeatureSpec "index" "" (define indexDef)

{-  
The baseline interval is the interval (b - 60, b), where b is the begin of 
index. Here, baseline is defined as function that takes a filtration function
as an argument, so that the baseline FeatureData can be used to filter events
based on different predicate functions.
-}
bline :: (IntervalSizeable a b) =>
     FeatureData (Events a)
  -> FeatureData (Interval a)
bline x = fmap (enderval 60 . begin) (eval indexDef x)

flwup :: (IntervalSizeable a b) =>
     FeatureData (Events a)
  -> FeatureData (Interval a)
flwup x = fmap (beginerval 30 . begin) (eval indexDef x)

{-
Define enrolled as the indicator of whether all of the gaps between the union of 
all enrollment intervals (+ allowableGap) 
-}
enrolled :: (IntervalSizeable a b) =>
      b
      -> Interval a -> Events a -> Bool
enrolled allowableGap i events =
    events
      |> makeConceptsFilter ["enrollment"]
      |> combineIntervals
      |> gapsWithin i
      |> maybe False (all (< allowableGap) . durations)

enrolledDef :: IntervalSizeable a b =>
      b
      -> FeatureDefinition
         (FeatureData (Interval a), FeatureData (Events a)) Bool
enrolledDef allowableGap = define2 (enrolled allowableGap)
{-
Define features that identify whether a subject as bit/struck by a duck and
bit/struck by a macaw.
-}
makeHxDef :: (Ord a) =>
         [Text]
      -> Interval a
      -> Events a
      -> (Bool, Maybe (Interval a))
makeHxDef cnpts i events =
   (isNotEmpty (f i events), lastMay $ intervals (f i events))
   where f i x = makePairedFilter enclose i (`hasConcepts` cnpts) x

duckHxDef :: (Ord a) =>
         Interval a
      -> Events a
      -> (Bool, Maybe (Interval a))
duckHxDef = makeHxDef ["wasBitByDuck", "wasStruckByDuck"]

macawHxDef :: (Ord a) =>
         Interval a
      -> Events a
      -> (Bool, Maybe (Interval a))
macawHxDef = makeHxDef ["wasBitByMacaw", "wasStruckByMacaw"]

-- | Define an event that identifies whether the subject has two minor or one major
--   surgery.
twoMinorOrOneMajorDef :: (Ord a) =>
         Interval a -> Events a ->  Bool
twoMinorOrOneMajorDef i events =
    twoXOrOneY ["hadMinorSurgery"] ["hadMajorSurgery"] (filterEnclose i events)

-- | Time from end of baseline to end of most recent Antibiotics
--   with 5 day grace period
timeSinceLastAntibioticsDef :: (IntervalSizeable a b) =>
      Interval a
      -> Events a
      -> Maybe b
timeSinceLastAntibioticsDef i  =
      lastMay                                 -- want the last one
    . map (max 0 . diff (end i) . end)        -- distances between end of baseline and antibiotic intervals
    . filterNotDisjoint i                     -- filter to intervals not disjoint from baseline interval
    . combineIntervals                        -- combine overlapping intervals
    . map (expandr 5)                         -- allow grace period
    . makeConceptsFilter ["tookAntibiotics"]  -- filter to only antibiotics events 

-- | Count of hospital events in a interval and duration of the last one
countOfHospitalEventsDef :: (IntervalSizeable a b) =>
      Interval a 
      -> Events a 
      -> (Int, Maybe b)
countOfHospitalEventsDef i =
       (\x -> (length x, duration <$> lastMay x))
    . filterNotDisjoint i                    -- filter to intervals not disjoint from interval
    . combineIntervals                       -- combine overlapping intervals
    . makeConceptsFilter ["wasHospitalized"]  -- filter to only antibiotics events

-- | time of distcontinuation of antibiotics
--   and time from start of follow up
--   This needs to be generalized as Nothing could either indicate they didn't 
--   discontinue or that they simply got no antibiotics records.
so :: Ord a => ComparativePredicateOf1 (Interval a)
so = unionPredicates [startedBy, overlappedBy]

discontinuationDef :: (IntervalSizeable a b) =>
      Interval a 
      -> Events a
      -> Maybe (a, b)
discontinuationDef i events = 
    (\x -> Just (begin x       -- we want the begin of this interval 
          , diff (begin x) (begin i)))
    =<< headMay                    -- if there are any gaps the first one is the first discontinuation
    =<< gapsWithin i               -- find gaps to intervals clipped to i
    =<< (nothingIfNone (so i)      -- if none of the intervals start or overlap 
                        -- the followup, then never started antibiotics
    . combineIntervals          -- combine overlapping intervals
    . map (expandr 5)           -- allow grace period
    . makeConceptsFilter        -- filter to only antibiotics events
          ["tookAntibiotics"])
    events


type MyData = 
     ( FeatureData (Interval Int)
     , FeatureData Bool
     , FeatureData (Bool, Maybe (Interval Int))
     , FeatureData (Bool, Maybe (Interval Int))
     , FeatureData Bool
     , FeatureData (Maybe Int)
     , FeatureData (Int, Maybe Int)
     , FeatureData (Maybe (Int, Int))
     )

getUnitFeatures ::
      Events Int
  -> MyData
getUnitFeatures x = (
    eval indexDef evs
  , eval (enrolledDef 8) (bline evs, evs)  
  , liftA2 duckHxDef  (bline evs) evs
  , liftA2 macawHxDef (bline evs) evs
  , liftA2 twoMinorOrOneMajorDef (bline evs) evs
  , liftA2 timeSinceLastAntibioticsDef (bline evs) evs
  , liftA2 countOfHospitalEventsDef (bline evs) evs
  , liftA2 discontinuationDef (flwup evs) evs
  ) where evs = pure x

example1results :: MyData
example1results =
      ( pure (beginerval 1 (60 :: Int))
      , pure True
      , pure (True, Just $ beginerval 1 (51 :: Int))
      , pure (False, Nothing)
      , pure True
      , pure $ Just 4
      , pure (1, Just 8)
      , pure $ Just (78, 18)
      )

example2results :: MyData
example2results = 
      ( featureDataL (Other "No occurrence of Orca bite")
      , featureDataL (Other "No occurrence of Orca bite")
      , featureDataL (Other "No occurrence of Orca bite")
      , featureDataL (Other "No occurrence of Orca bite")
      , featureDataL (Other "No occurrence of Orca bite")
      , featureDataL (Other "No occurrence of Orca bite")
      , featureDataL (Other "No occurrence of Orca bite")
      , featureDataL (Other "No occurrence of Orca bite")
      )

exampleFeatures1Spec :: Spec
exampleFeatures1Spec = do

    it "getUnitFeatures from exampleEvents1" $
      getUnitFeatures exampleEvents1 `shouldBe` example1results

    it "getUnitFeatures from exampleEvents2" $
      getUnitFeatures exampleEvents2 `shouldBe` example2results

    it "mapping a population to cohort" $
      makeCohort getUnitFeatures (MkPopulation [exampleSubject1, exampleSubject2 ]) `shouldBe`
            MkCohort [MkObsUnit ("a", example1results), MkObsUnit ("b", example2results)]

