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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ExampleFeatures1
  ( exampleFeatures1Spec
  ) where

import           ExampleEvents
import           Hasklepias
import           Test.Hspec
import           Cohort.Attrition -- imported for test case
{-
Index is defined as the first occurrence of an Orca bite.
-}
defineIndexSet :: Ord a => Events a -> IndexSet Interval a
defineIndexSet events =
  makeIndexSet $
     makeIndex . getInterval <$>
     makeConceptsFilter ["wasBitByOrca"] events

{-  
The baseline interval is the interval (b - 60, b), where b is the begin of 
index. Here, baseline is defined as function that takes a filtration function
as an argument, so that the baseline FeatureData can be used to filter events
based on different predicate functions.
-}
bline :: (IntervalSizeable a b) => Index Interval a -> AssessmentInterval a
bline = makeBaselineFromIndex 60

flwup :: (IntervalSizeable a b) => Index Interval a -> AssessmentInterval a
flwup = makeFollowupFromIndex 30

{-
Define features that identify whether a subject as bit/struck by a duck and
bit/struck by a macaw.
-}
makeHx
  :: (Ord a)
  => [Text]
  -> AssessmentInterval a
  -> Events a
  -> (Bool, Maybe (Interval a))
makeHx cnpts i events =
  (isNotEmpty (f i events), lastMay $ intervals (f i events))
  where f i = makePairedFilter enclose i (`hasConcepts` cnpts)

duckHx
  :: (Ord a) => AssessmentInterval a -> Events a -> (Bool, Maybe (Interval a))
duckHx = makeHx ["wasBitByDuck", "wasStruckByDuck"]

duckHxDef
  :: (Ord a)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" (Events a)
       -> Feature "duck history" (Bool, Maybe (Interval a))
       )
duckHxDef = define duckHx

macawHx
  :: (Ord a) => AssessmentInterval a -> Events a -> (Bool, Maybe (Interval a))
macawHx = makeHx ["wasBitByMacaw", "wasStruckByMacaw"]

macawHxDef
  :: (Ord a)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" (Events a)
       -> Feature "macaw history" (Bool, Maybe (Interval a))
       )
macawHxDef = define macawHx

-- | a helper function for 'twoMinorOrOneMajorDef' 
twoXOrOneY :: [Text] -> [Text] -> Events a -> Bool
twoXOrOneY x y es = atleastNofX 2 x es || atleastNofX 1 y es

-- | Define an event that identifies whether the subject has two minor or one major
--   surgery.
twoMinorOrOneMajor :: (Ord a) => AssessmentInterval a -> Events a -> Bool
twoMinorOrOneMajor i events =
  twoXOrOneY ["hadMinorSurgery"] ["hadMajorSurgery"] (filterEnclose i events)

twoMinorOrOneMajorDef
  :: (Ord a)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" (Events a)
       -> Feature "two major or one minor" Bool
       )
twoMinorOrOneMajorDef = define twoMinorOrOneMajor

-- | Time from end of baseline to end of most recent Antibiotics
--   with 5 day grace period
timeSinceLastAntibiotics
  :: (IntervalSizeable a b) => AssessmentInterval a -> Events a -> Maybe b
timeSinceLastAntibiotics i =
  lastMay                                 -- want the last one
    . map (max 0 . diff (end i) . end)        -- distances between end of baseline and antibiotic intervals
    . filterNotDisjoint i                     -- filter to intervals not disjoint from baseline interval
    . combineIntervals                        -- combine overlapping intervals
    . map (expandr 5)                         -- allow grace period
    . makeConceptsFilter ["tookAntibiotics"]  -- filter to only antibiotics events 

timeSinceLastAntibioticsDef
  :: (IntervalSizeable a b)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" (Events a)
       -> Feature "time since antibiotics" (Maybe b)
       )
timeSinceLastAntibioticsDef = define timeSinceLastAntibiotics


-- | Count of hospital events in a interval and duration of the last one
countOfHospitalEvents
  :: (IntervalSizeable a b)
  => AssessmentInterval a
  -> Events a
  -> (Int, Maybe b)
countOfHospitalEvents i =
  (\x -> (length x, duration <$> lastMay x))
    . filterNotDisjoint i                    -- filter to intervals not disjoint from interval
    . combineIntervals                       -- combine overlapping intervals
    . makeConceptsFilter ["wasHospitalized"]  -- filter to only antibiotics events

countOfHospitalEventsDef
  :: (IntervalSizeable a b)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" (Events a)
       -> Feature "count of hospitalizations" (Int, Maybe b)
       )
countOfHospitalEventsDef = define countOfHospitalEvents

-- | time of distcontinuation of antibiotics
--   and time from start of follow up
--   This needs to be generalized as Nothing could either indicate they didn't 
--   discontinue or that they simply got no antibiotics records.
so :: Intervallic i a => ComparativePredicateOf1 (i a)
so = unionPredicates [startedBy, overlappedBy]

discontinuation
  :: (IntervalSizeable a b) => AssessmentInterval a -> Events a -> Maybe (a, b)
discontinuation i events =
  (\x -> Just
      ( begin x       -- we want the begin of this interval 
      , diff (begin x) (begin i)
      )
    )
    =<< headMay                    -- if there are any gaps the first one is the first discontinuation
    =<< gapsWithin i               -- find gaps to intervals clipped to i
    =<< ( nothingIfNone (so (getInterval i))      -- if none of the intervals start or overlap 
                        -- the followup, then never started antibiotics
        . combineIntervals          -- combine overlapping intervals
        . map (expandr 5)           -- allow grace period
        . makeConceptsFilter        -- filter to only antibiotics events
                             ["tookAntibiotics"]
        )
          events

discontinuationDef
  :: (IntervalSizeable a b)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" (Events a)
       -> Feature "discontinuation" (Maybe (a, b))
       )
discontinuationDef = define discontinuation

{-
  Tests
-}

type MyData
  = ( Feature "index" (Index Interval Int)
    , Feature "enrolled" Status
    , Feature "duck history" (Bool, Maybe (Interval Int))
    , Feature "macaw history" (Bool, Maybe (Interval Int))
    , Feature "two major or one minor" Bool
    , Feature "time since antibiotics" (Maybe Int)
    , Feature "count of hospitalizations" (Int, Maybe Int)
    , Feature "discontinuation" (Maybe (Int, Int))
    )

getUnitFeatures :: Index Interval Int -> Events Int -> MyData
getUnitFeatures index x =
  ( idx
  , eval
    (buildContinuousEnrollment bline (containsConcepts ["enrollment"]) 60)
    idx
    evs
    (pure Include)
  , eval duckHxDef                   bl evs
  , eval macawHxDef                  bl evs
  , eval twoMinorOrOneMajorDef       bl evs
  , eval timeSinceLastAntibioticsDef bl evs
  , eval countOfHospitalEventsDef    bl evs
  , eval discontinuationDef          fl evs
  )
 where
  evs = pure x
  idx = pure index
  bl  = fmap bline idx
  fl  = fmap flwup idx

-- just a dummy set for now
dummyIndex :: Index Interval Int
dummyIndex = makeIndex $ beginerval 1 0

includeAll :: Index Interval Int -> Events Int -> Criteria
includeAll _ _ = criteria $ pure
  (criterion (makeFeature (featureDataR Include) :: Feature "includeAll" Status)
  )

testCohortSpec :: CohortSpec (Events Int) MyData Interval Int
testCohortSpec = specifyCohort defineIndexSet includeAll getUnitFeatures

example1results :: MyData
example1results =
  ( pure $ makeIndex (beginerval 1 (60 :: Int))
  , pure Include
  , pure (True, Just $ beginerval 1 (51 :: Int))
  , pure (False, Nothing)
  , pure True
  , pure $ Just 4
  , pure (1, Just 8)
  , pure $ Just (78, 18)
  )


exampleFeatures1Spec :: Spec
exampleFeatures1Spec = do

  it "getUnitFeatures from exampleEvents1"
    $          getUnitFeatures (makeIndex (beginerval 1 60)) exampleEvents1
    `shouldBe` example1results

  it "mapping a population to cohort"
    $          evalCohort testCohortSpec
                          (MkPopulation [exampleSubject1, exampleSubject2])
    `shouldBe` MkCohort
                 ( MkAttritionInfo 2 $ setFromList
                   [ MkAttritionLevel SubjectHasNoIndex 1
                   , MkAttritionLevel (ExcludedBy (1, "includeAll")) 0
                   , MkAttritionLevel Included 1
                   ]
                 , MkCohortData
                   [ MkObsUnit (makeObsID 1 "a") example1results
                   ]
                 )

