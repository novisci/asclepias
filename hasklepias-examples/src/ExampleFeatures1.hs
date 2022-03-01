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

import           EventData                      ( containsConcepts )
import           Hasklepias
import           Templates.Features
import           Test.Hspec  -- imported for test case
{-
Index is defined as the first occurrence of an Orca bite.
-}
defineIndexSet :: Ord a => [Event ClaimsSchema Text a] -> IndexSet (Interval a)
defineIndexSet events =
  makeIndexSet
    $   getInterval
    <$> filterEvents (containsConcepts ["wasBitByOrca"]) events

{-  
The baseline interval is the interval (b - 60, b), where b is the begin of 
index. Here, baseline is defined as function that takes a filtration function
as an argument, so that the baseline FeatureData can be used to filter events
based on different predicate functions.
-}
bline :: (IntervalSizeable a b) => Interval a -> Interval a
bline = getInterval . makeBaselineFromIndex 60

flwup :: (IntervalSizeable a b) => Interval a -> Interval a
flwup = getInterval . makeFollowupFromIndex 30

{-
Define features that identify whether a subject was bit/struck by a duck and
bit/struck by a macaw.
-}
makeHx
  :: [Text]
  -> Interval Day
  -> [Event ClaimsSchema Text Day]
  -> (Bool, Maybe (Interval Day))
makeHx cnpts i events =
  (isNotEmpty (f i events'), lastMay $ intervals (f i events'))
  -- TODO: find the available functions to replace this mess
 where
  f i = makePairedFilter enclose i (`hasConcepts` cnpts)
  hasConcepts x = any (\c -> x `hasConcept` c)
  makePairedFilter fi i fc = filter (makePairPredicate fi i fc)
  makePairPredicate pi i pd x = pi i x && pd (getPairData x)
  events' = map getEvent events

duckHx
  :: Interval Day
  -> [Event ClaimsSchema Text Day]
  -> (Bool, Maybe (Interval Day))
duckHx = makeHx ["wasBitByDuck", "wasStruckByDuck"]

duckHxDef
  :: Definition
       (  Feature "index" (Interval Day)
       -> Feature "events" [Event ClaimsSchema Text Day]
       -> Feature "duck history" (Bool, Maybe (Interval Day))
       )
duckHxDef = define duckHx

macawHx
  :: Interval Day
  -> [Event ClaimsSchema Text Day]
  -> (Bool, Maybe (Interval Day))
macawHx = makeHx ["wasBitByMacaw", "wasStruckByMacaw"]

macawHxDef
  :: Definition
       (  Feature "index" (Interval Day)
       -> Feature "events" [Event ClaimsSchema Text Day]
       -> Feature "macaw history" (Bool, Maybe (Interval Day))
       )
macawHxDef = define macawHx

baselineInterval :: Interval Day -> AssessmentInterval Day
baselineInterval = makeBaselineFromIndex 455

-- NOTE copied from ExampleCohort1.hs

-- | Defines a feature that returns 'True' ('False' otherwise) if either:
--   * at least 1 event during the baseline interval has any of the 'cpts1' concepts
--   * there are at least 2 events that have 'cpts2' concepts which have at least
--     7 days between them during the baseline interval
twoMinorOrOneMajorDef
  :: Definition
       (  Feature "calendarIndex" (Interval Day)
       -> Feature "events" [Event ClaimsSchema Text Day]
       -> Feature name Bool
       )
twoMinorOrOneMajorDef = buildNofXOrNofYWithGapBool
  1
  (containsConcepts ["hadMinorSurgery"])
  0
  7
  baselineInterval
  concur
  (containsConcepts ["hadMajorSurgery"])

---- | a helper function for 'twoMinorOrOneMajorDef' 
--twoXOrOneY :: [Text] -> [Text] -> [Event ClaimsSchema Text a] -> Bool
--twoXOrOneY x y es = atleastNofX 2 x es || atleastNofX 1 y es
--
---- | Define an event that identifies whether the subject has two minor or one major
----   surgery.
--twoMinorOrOneMajor
--  :: (Ord a) => AssessmentInterval a -> [Event ClaimsSchema Text a] -> Bool
--twoMinorOrOneMajor i events =
--  twoXOrOneY ["hadMinorSurgery"] ["hadMajorSurgery"] (filterEnclose i events)

--twoMinorOrOneMajorDef
--  :: (Ord a)
--  => Definition
--       (  Feature "index" (AssessmentInterval a)
--       -> Feature "events" [Event ClaimsSchema Text a]
--       -> Feature "two major or one minor" Bool
--       )
--twoMinorOrOneMajorDef = define twoMinorOrOneMajor

-- | Time from end of baseline to end of most recent Antibiotics
--   with 5 day grace period
--timeSinceLastAntibiotics
--  :: (IntervalSizeable a b)
--  => AssessmentInterval a
--  -> [Event ClaimsSchema Text a]
--  -> Maybe b
--timeSinceLastAntibiotics i =
--  lastMay                                 -- want the last one
--    . map (max 0 . diff (end i) . end)        -- distances between end of baseline and antibiotic intervals
--    . filterNotDisjoint i                     -- filter to intervals not disjoint from baseline interval
--    . combineIntervals                        -- combine overlapping intervals
--    . map (expandr 5)                         -- allow grace period
--    . makeConceptsFilter ["tookAntibiotics"]  -- filter to only antibiotics events 

--timeSinceLastAntibioticsDef
--  :: (IntervalSizeable a b)
--  => Definition
--       (  Feature "index" (AssessmentInterval a)
--       -> Feature "events" [Event ClaimsSchema Text a]
--       -> Feature "time since antibiotics" (Maybe b)
--       )
--timeSinceLastAntibioticsDef = define timeSinceLastAntibiotics


-- | Count of hospital events in a interval and duration of the last one
--countOfHospitalEvents
--  :: (IntervalSizeable a b)
--  => AssessmentInterval a
--  -> [Event ClaimsSchema Text a]
--  -> (Int, Maybe b)
--countOfHospitalEvents i =
--  (\x -> (length x, duration <$> lastMay x))
--    . filterNotDisjoint i                    -- filter to intervals not disjoint from interval
--    . combineIntervals                       -- combine overlapping intervals
--    . makeConceptsFilter ["wasHospitalized"]  -- filter to only antibiotics events

--countOfHospitalEventsDef
--  :: (IntervalSizeable a b)
--  => Definition
--       (  Feature "index" (AssessmentInterval a)
--       -> Feature "events" [Event ClaimsSchema Text a]
--       -> Feature "count of hospitalizations" (Int, Maybe b)
--       )
--countOfHospitalEventsDef = define countOfHospitalEvents

-- | time of distcontinuation of antibiotics
--   and time from start of follow up
--   This needs to be generalized as Nothing could either indicate they didn't 
--   discontinue or that they simply got no antibiotics records.
so :: Intervallic i a => ComparativePredicateOf1 (i a)
so = unionPredicates [startedBy, overlappedBy]

--discontinuation
--  :: (IntervalSizeable a b)
--  => AssessmentInterval a
--  -> [Event ClaimsSchema Text a]
--  -> Maybe (a, b)
--discontinuation i events =
--  (\x -> Just
--      ( begin x       -- we want the begin of this interval 
--      , diff (begin x) (begin i)
--      )
--    )
--    =<< headMay                    -- if there are any gaps the first one is the first discontinuation
--    =<< gapsWithin i               -- find gaps to intervals clipped to i
--    =<< ( nothingIfNone (so (getInterval i))      -- if none of the intervals start or overlap 
--                        -- the followup, then never started antibiotics
--        . combineIntervals          -- combine overlapping intervals
--        . map (expandr 5)           -- allow grace period
--        . makeConceptsFilter        -- filter to only antibiotics events
--                             ["tookAntibiotics"]
--        )
--          events

--discontinuationDef
--  :: (IntervalSizeable a b)
--  => Definition
--       (  Feature "index" (AssessmentInterval a)
--       -> Feature "events" [Event ClaimsSchema Text a]
--       -> Feature "discontinuation" (Maybe (a, b))
--       )
--discontinuationDef = define discontinuation

{-
  Tests
-}

type MyData
  = ( Feature "index" (Interval Day)
    --, Feature "enrolled" Status
    , Feature "duck history" (Bool, Maybe (Interval Day))
    , Feature "macaw history" (Bool, Maybe (Interval Day))
    , Feature "two major or one minor" Bool
--    , Feature "time since antibiotics" (Maybe Int)
--    , Feature "count of hospitalizations" (Int, Maybe Int)
--    , Feature "discontinuation" (Maybe (Int, Int))
    )

getUnitFeatures :: Interval Day -> [Event ClaimsSchema Text Day] -> MyData
getUnitFeatures index x =
  ( idx
  --, eval
  --  (buildContinuousEnrollment bline (containsConcepts ["enrollment"]) 60)
  --  idx
  --  evs
  --  (pure Include)
  , eval duckHxDef                   bl evs
  , eval macawHxDef                  bl evs
  , eval twoMinorOrOneMajorDef       bl' evs
--  , eval timeSinceLastAntibioticsDef bl evs
--  , eval countOfHospitalEventsDef    bl evs
--  , eval discontinuationDef          fl evs
  )
 where
  evs = pure x
  idx = pure index
  -- TODO: i broke it
  idx' = pure index
  bl' = fmap bline idx'
  bl  = fmap bline idx
  fl  = fmap flwup idx

-- just a dummy set for now
dummyIndex :: Interval Int
dummyIndex = beginerval 1 0

includeAll :: Interval Int -> [Event ClaimsSchema Text Int] -> Criteria
includeAll _ _ = criteria $ pure
  (criterion (makeFeature (featureDataR Include) :: Feature "includeAll" Status)
  )

testCohortSpec
  :: CohortSpec [Event ClaimsSchema Text Int] MyData (Interval Int)
testCohortSpec = specifyCohort defineIndexSet includeAll getUnitFeatures

example1results :: MyData
example1results =
  ( pure (beginerval 1 (60 :: Int))
  , pure Include
  , pure (True, Just $ beginerval 1 (51 :: Int))
  , pure (False, Nothing)
  , pure True
--  , pure $ Just 4
--  , pure (1, Just 8)
--  , pure $ Just (78, 18)
  )


exampleFeatures1Spec :: Spec
exampleFeatures1Spec = do

  it "getUnitFeatures from exampleEvents1"
    $          getUnitFeatures (beginerval 1 60) exampleEvents1
    `shouldBe` example1results

  it "mapping a population to cohort"
    $          evalCohort testCohortSpec
                          (MkPopulation [exampleSubject1, exampleSubject2])
    `shouldBe` MkCohort
                 ( MkAttritionInfo
                   2
                   2
                   setFromList
                   [ MkAttritionLevel SubjectHasNoIndex              1
                   , MkAttritionLevel (ExcludedBy (1, "includeAll")) 0
                   , MkAttritionLevel Included                       1
                   ]
                 , MkCohortData [MkObsUnit (makeObsID 1 "a") example1results]
                 )

