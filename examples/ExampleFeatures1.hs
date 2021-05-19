{-|
Module      : ExampleFeatures1
Description : Demostrates how to define features using Hasklepias
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ExampleFeatures1(
    exampleFeatures1Spec
) where

import Hasklepias
import ExampleEvents
import Test.Hspec


{-
Index is defined as the first occurrence of an Orca bite.
-}
indexDef :: (Intervallic Interval a) =>
          FeatureDefinition e a (Interval a)
indexDef = defineEF
      (Other "No occurrence of Orca bite")
      (firstConceptOccurrence ["wasBitByOrca"])
      getInterval

{-  
The baseline interval is the interval (b - 60, b), where b is the begin of 
index. Here, baseline is defined as function that takes a filtration function
as an argument, so that the baseline feature can be used to filter events
based on different predicate functions.
-}
baseline :: (Intervallic Interval a, IntervalSizeable a b) =>
     Feature (Interval a) -- ^ pass the result of index to get a baseline filter
  -> Feature (Interval a)
baseline = fmap (enderval 60 . begin)

bline :: (Intervallic Interval a, IntervalSizeable a b) =>
     Events a
  -> Feature (Interval a)
bline = baseline . applyEF indexDef

flwup :: (Intervallic Interval a, IntervalSizeable a b) =>
     Events a
  -> Feature (Interval a)
flwup = fmap (beginerval 30 . begin) . applyEF indexDef

{-
Define enrolled as the indicator of whether all of the gaps between the union of 
all enrollment intervals (+ allowableGap) 
-}
enrolledDef :: IntervalSizeable a b => b -> FeatureDefinition (Interval a) a Bool
enrolledDef allowableGap = defineFEF Excluded
   (  \i ->
        maybe False (all (< allowableGap) . durations)
      . gapsWithin i
      . combineIntervals
      . intervals
      . makeConceptsFilter ["enrollment"]
   )

{-
Define features that identify whether a subject as bit/struck by a duck and
bit/struck by a macaw.
-}
makeHxDef :: (Intervallic Interval a) =>
               [Text] -> FeatureDefinition (Interval a) a (Bool, Maybe (Interval a))
makeHxDef cnpts = defineFEF Excluded
   ( \i es ->
      (isNotEmpty (f i es), lastMay $ intervals (f i es))
   )
   where f i x = makePairedFilter enclose i (`hasConcepts` cnpts) x

duckHxDef :: (Intervallic Interval a) =>
          FeatureDefinition (Interval a) a (Bool, Maybe (Interval a))
duckHxDef = makeHxDef ["wasBitByDuck", "wasStruckByDuck"]

macawHxDef :: (Intervallic Interval a) =>
          FeatureDefinition (Interval a) a (Bool, Maybe (Interval a))
macawHxDef = makeHxDef ["wasBitByMacaw", "wasStruckByMacaw"]

-- | Define an event that identifies whether the subject has two minor or one major
--   surgery.
twoMinorOrOneMajorDef :: (Intervallic Interval a) =>
         FeatureDefinition (Interval a) a Bool
twoMinorOrOneMajorDef = defineFEF Excluded
      ( \i es ->
          twoXOrOneY ["hadMinorSurgery"] ["hadMajorSurgery"] (filterEnclose i es)
      )

-- | Time from end of baseline to end of most recent Antibiotics
--   with 5 day grace period
timeSinceLastAntibioticsDef :: ( Intervallic Interval a
                               , IntervalSizeable a b) =>
         FeatureDefinition (Interval a) a (Maybe b)
timeSinceLastAntibioticsDef = defineFEF Excluded
      ( \i es ->
           ( lastMay                                -- want the last one
           . map (max 0 . diff (end i) . end)       -- distances between end of baseline and antibiotic intervals
           . filterNotDisjoint i                    -- filter to intervals not disjoint from baseline interval
           . combineIntervals                       -- combine overlapping intervals
           . map (expandr 5)                        -- allow grace period
           . intervals                              -- extract intervals
           . makeConceptsFilter ["tookAntibiotics"])-- filter to only antibiotics events 
            es
      )

-- | Count of hospital events in a interval and duration of the last one
countOfHospitalEventsDef :: (IntervalCombinable Interval a
                            , IntervalSizeable a b) =>
                            FeatureDefinition (Interval a) a (Int, Maybe b)
countOfHospitalEventsDef = defineFEF Excluded
      ( \i es ->
            ((\x -> (length x, duration <$> lastMay x))
            .filterNotDisjoint i                    -- filter to intervals not disjoint from interval
            .combineIntervals                       -- combine overlapping intervals
            .intervals                              -- extract intervals
            .makeConceptsFilter ["wasHospitalized"])  -- filter to only antibiotics events
            es
      )

-- | time of distcontinuation of antibiotics
--   and time from start of follow up
--   This needs to be generalized as Nothing could either indicate they didn't 
--   discontinue or that they simply got no antibiotics records.
so :: (Intervallic Interval a)=> ComparativePredicateOf1 (Interval a)
so = unionPredicates [startedBy, overlappedBy]

discontinuationDef :: (IntervalSizeable a b
                      , Intervallic Interval a) =>
                      FeatureDefinition (Interval a) a (Maybe (a, b))
discontinuationDef = defineFEF Excluded
      ( \i es ->
          (\x -> Just (begin x       -- we want the begin of this interval 
                 , diff (begin x) (begin i)))
      =<< headMay                    -- if there are any gaps the first one is the first discontinuation
      =<< gapsWithin i               -- find gaps to intervals clipped to i
      =<< (nothingIfNone (so i)      -- if none of the intervals start or overlap 
                                     -- the followup, then never started antibiotics
         . combineIntervals          -- combine overlapping intervals
         . map (expandr 5)           -- allow grace period
         . intervals                 -- extract intervals
         . makeConceptsFilter        -- filter to only antibiotics events
            ["tookAntibiotics"]) es
      )

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
    applyEF  indexDef x
  , applyFEF (enrolledDef 8) (bline x) x
  , applyFEF duckHxDef (bline x) x
  , applyFEF macawHxDef (bline x) x
  , applyFEF twoMinorOrOneMajorDef (bline x) x
  , applyFEF timeSinceLastAntibioticsDef (bline x) x
  , applyFEF countOfHospitalEventsDef (bline x) x
  , applyFEF discontinuationDef (flwup x) x
  )

exampleFeatures1Spec :: Spec
exampleFeatures1Spec = do

    it "getUnitFeatures from exampleEvents1" $
      getUnitFeatures exampleEvents1 `shouldBe`
      ( featureR (beginerval 1 (60 :: Int))
      , featureR True
      , featureR (True, Just $ beginerval 1 (51 :: Int))
      , featureR (False, Nothing)
      , featureR True
      , featureR $ Just 4
      , featureR (1, Just 8)
      , featureR $ Just (78, 18)
      )

    it "getUnitFeatures from exampleEvents2" $
      getUnitFeatures exampleEvents2 `shouldBe`
      ( featureL (Other "No occurrence of Orca bite")
      , featureL Excluded
      , featureL Excluded
      , featureL Excluded
      , featureL Excluded
      , featureL Excluded
      , featureL Excluded
      , featureL Excluded
      )
