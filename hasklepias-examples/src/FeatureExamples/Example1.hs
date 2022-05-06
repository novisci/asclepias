{-|
Description : Demostrates how to define features using Hasklepias
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module FeatureExamples.Example1
  ( example
  ) where

import           ExampleEvents
import           Hasklepias

{-
Index is defined as the first occurrence of an Orca bite.
-}
defineIndexSet :: Ord a => [Event Text ClaimsSchema a] -> IndexSet (Interval a)
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
bline :: (IntervalSizeable a b) => Interval a -> AssessmentInterval a
bline = makeBaselineFromIndex 60

flwup :: (IntervalSizeable a b) => Interval a -> AssessmentInterval a
flwup = makeFollowupFromIndex 30

-- | TODO
atleastNofX :: (Ord c) => Int -> [c] -> [Event c d a] -> Bool
atleastNofX n xs es = tallyEvents (Predicate (`hasAnyConcepts` xs)) es >= n

-- | TODO
makeConceptsFilter
  :: (Filterable f, Ord c) => [c] -> f (Event c d a) -> f (Event c d a)
makeConceptsFilter cpts = filter (`hasAnyConcepts` cpts)

{-
Define features that identify whether a subject was bit/struck by a duck and
bit/struck by a macaw.
-}
makeHx
  :: (Ord a)
  => [Text]
  -> AssessmentInterval a
  -> [Event Text ClaimsSchema a]
  -> (Bool, Maybe (Interval a))
makeHx cnpts i events =
  (isNotEmpty (f i events'), lastMay $ intervals (f i events'))
  -- TODO: find the available functions to replace this mess
 where
  f i = makePairedFilter enclose i (`hasAnyConcepts` cnpts)
  makePairedFilter fi i fc = filter (makePairPredicate fi i fc)
  makePairPredicate pi i pd x = pi i x && pd (getPairData x)
  events' = map getEvent events

duckHx
  :: (Ord a)
  => AssessmentInterval a
  -> [Event Text ClaimsSchema a]
  -> (Bool, Maybe (Interval a))
duckHx = makeHx ["wasBitByDuck", "wasStruckByDuck"]

duckHxDef
  :: (Ord a)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" [Event Text ClaimsSchema a]
       -> Feature "duck history" (Bool, Maybe (Interval a))
       )
duckHxDef = define duckHx

macawHx
  :: (Ord a)
  => AssessmentInterval a
  -> [Event Text ClaimsSchema a]
  -> (Bool, Maybe (Interval a))
macawHx = makeHx ["wasBitByMacaw", "wasStruckByMacaw"]

macawHxDef
  :: (Ord a)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" [Event Text ClaimsSchema a]
       -> Feature "macaw history" (Bool, Maybe (Interval a))
       )
macawHxDef = define macawHx

-- | a helper function for 'twoMinorOrOneMajorDef' 
twoXOrOneY :: [Text] -> [Text] -> [Event Text ClaimsSchema a] -> Bool
twoXOrOneY x y es = atleastNofX 2 x es || atleastNofX 1 y es

-- | Define an event that identifies whether the subject has two minor or one major
--   surgery.
twoMinorOrOneMajor
  :: (Ord a) => AssessmentInterval a -> [Event Text ClaimsSchema a] -> Bool
twoMinorOrOneMajor i events =
  twoXOrOneY ["hadMinorSurgery"] ["hadMajorSurgery"] (filterEnclose i events)

twoMinorOrOneMajorDef
  :: (Ord a)
  => Definition
       (  Feature "index" (AssessmentInterval a)
       -> Feature "events" [Event Text ClaimsSchema a]
       -> Feature "two major or one minor" Bool
       )
twoMinorOrOneMajorDef = define twoMinorOrOneMajor

-- | Time from end of baseline to end of most recent Antibiotics
--   with 5 day grace period
timeSinceLastAntibiotics
  :: (IntervalSizeable a b)
  => AssessmentInterval a
  -> [Event Text ClaimsSchema a]
  -> Maybe b
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
       -> Feature "events" [Event Text ClaimsSchema a]
       -> Feature "time since antibiotics" (Maybe b)
       )
timeSinceLastAntibioticsDef = define timeSinceLastAntibiotics


-- | Count of hospital events in a interval and duration of the last one
countOfHospitalEvents
  :: (IntervalSizeable a b)
  => AssessmentInterval a
  -> [Event Text ClaimsSchema a]
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
       -> Feature "events" [Event Text ClaimsSchema a]
       -> Feature "count of hospitalizations" (Int, Maybe b)
       )
countOfHospitalEventsDef = define countOfHospitalEvents

-- | time of distcontinuation of antibiotics
--   and time from start of follow up
--   TODO This needs to be generalized as Nothing could either indicate they didn't 
--   discontinue or that they simply got no antibiotics records.
so :: Intervallic i a => ComparativePredicateOf1 (i a)
so = unionPredicates [startedBy, overlappedBy]

discontinuation
  :: (IntervalSizeable a b)
  => AssessmentInterval a
  -> [Event Text ClaimsSchema a]
  -> Maybe (a, b)
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
       -> Feature "events" [Event Text ClaimsSchema a]
       -> Feature "discontinuation" (Maybe (a, b))
       )
discontinuationDef = define discontinuation

{-
  Tests
-}

type MyData
  = ( Feature "index" (Interval Int)
    , Feature "enrolled" Status
    , Feature "duck history" (Bool, Maybe (Interval Int))
    , Feature "macaw history" (Bool, Maybe (Interval Int))
    , Feature "two major or one minor" Bool
    , Feature "time since antibiotics" (Maybe Int)
    , Feature "count of hospitalizations" (Int, Maybe Int)
    , Feature "discontinuation" (Maybe (Int, Int))
    )

getUnitFeatures :: Interval Int -> [Event Text ClaimsSchema Int] -> MyData
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
dummyIndex :: Interval Int
dummyIndex = beginerval 1 0

includeAll :: Interval Int -> [Event Text ClaimsSchema Int] -> Criteria
includeAll _ _ = criteria $ pure
  (criterion (makeFeature (featureDataR Include) :: Feature "includeAll" Status)
  )


example1results :: MyData
example1results =
  ( pure (beginerval 1 (60 :: Int))
  , pure Include
  , pure (True, Just $ beginerval 1 (45 :: Int))
  , pure (False, Nothing)
  , pure True
  , pure $ Just 4
  , pure (1, Just 8)
  , pure $ Just (78, 18)
  )

exampleEvalOpts :: CohortEvalOptions
exampleEvalOpts = defaultCohortEvalOptions

exampleCohortSpec
  :: CohortSpec [Event Text ClaimsSchema Int] MyData (Interval Int)
exampleCohortSpec = specifyCohort defineIndexSet includeAll getUnitFeatures

-- NOTE makeCohortEvaluator requires a monad wrapper in return type. Using
-- Either here because IO a has no Eq instance, hence testing doesn't work.
exampleCohortEvaluator
  :: Population [Event Text ClaimsSchema Int]
  -> Either Text (Cohort MyData (Interval Int))
exampleCohortEvaluator = makeCohortEvaluator exampleEvalOpts exampleCohortSpec

-- NOTE constructor unexported.  only way to do construct it is with `from`. 
examplePopulation :: Population [Event Text ClaimsSchema Int]
examplePopulation = from [exampleSubject1, exampleSubject2]

example :: TestTree
example = testGroup
  ""
  [ testCase "getUnitFeatures from exampleEvents1"
  $   getUnitFeatures (beginerval 1 60) exampleEvents1
  @?= example1results
  , testCase "mapping population to cohort"
  $   exampleCohortEvaluator examplePopulation
  @?= Right
        (MkCohort
          ( makeTestAttritionInfo
            2
            1
            [ (SubjectHasNoIndex           , 1)
            , (ExcludedBy (1, "includeAll"), 0)
            , (Included                    , 1)
            ]
          , from @[ObsUnit MyData (Interval Int)]
            [ from @(ObsID (Interval Int), MyData)
                (makeObsID (beginervalMoment 60) ("a" :: Text), example1results)
            ]
          )
        )
  ]
