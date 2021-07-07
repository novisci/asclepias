{-|
Module      : ExampleCohort1
Description : Demostrates how to define a cohort using Hasklepias
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module ExampleCohort1(
  exampleCohort1tests
) where
import Hasklepias
{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

-- | Lookback duration for baseline
baselineLookback :: Integer
baselineLookback = 455

-- | Duration of follow up in months
followupDuration :: CalendarDiffDays
followupDuration = CalendarDiffDays 3 0

-- | Calendar indices: first day of each quarter for 2017-2018
indices :: [Index Interval Day]
indices =  map (\(y, m) -> makeIndex $ beginerval 0 (fromGregorian y m 1))
           (allPairs [2017..2018] [1, 4, 7, 10])

{-------------------------------------------------------------------------------
  Utility functions 
-------------------------------------------------------------------------------}

-- | Creates a baseline interval from index
baselineInterval :: Index Interval Day -> Interval Day
baselineInterval index = lookback baselineLookback (getIndex index)

-- | Shifts an interval by a calendar amount
shiftIntervalDay :: CalendarDiffDays -> Interval Day -> Interval Day
shiftIntervalDay cd i = beginerval (duration i) (addGregorianDurationClip cd (begin i))

-- | Creates an interval *beginning the same day as the index* and 
--   ending 'followupDuration' days later.
followupInterval :: Index Interval Day -> Interval Day
followupInterval index = beginerval (diff bi (end $ shiftIntervalDay followupDuration i)) bi
    where i = getIndex index
          bi = begin i

-- | A predicate function that determines if some interval is before index
beforeIndex :: Intervallic i Day => Index Interval Day -> i Day -> Bool
beforeIndex index = before (getIndex index)

-- | Creates a filter for events to those that 'concur' with the baseline interval.
getBaselineConcur ::  Index Interval Day -> Events Day -> [Event Day]
getBaselineConcur index = filterConcur (baselineInterval index)

{-------------------------------------------------------------------------------
  Feature patterns: functions for defining features by a given pattern 
-------------------------------------------------------------------------------}

-- | Defines a feature that returns 'True' ('False' otherwise) if either:
--   * at least 1 event during the baseline interval has any of the 'c1' concepts
--   * there are at least 2 event that have 'c2' concepts which have at least
--     7 days between them during the baseline interval
twoOutOneIn ::
       [Text]
    -> [Text]
    -> Definition
    ( Feature "calendarIndex" (Index Interval Day)
    -> Feature "allEvents" (Events Day)
    -> Feature name Bool )
twoOutOneIn cpts1 cpts2 = define
    (\index events ->
        atleastNofX 1 cpts1  (getBaselineConcur index events) 
        || ( 
          events 
        |> makeConceptsFilter cpts2 
        |> map toConceptEvent
        |> anyGapsWithinAtLeastDuration 7 (baselineInterval index)) 
          
    )

-- | Defines a feature that returns 'True' ('False' otherwise) if either:
--   * any events concuring with baseline with concepts in 'cpts' have a 
--     duration >= 90 days
--   * at least 2 events with concepts in 'cpts' have the same interval 
medHx :: [Text]
  -> Definition
 ( Feature "calendarIndex" (Index Interval Day)
  -> Feature "allEvents" (Events Day)
  -> Feature name Bool )
medHx cpt = define
    (\index events ->
            ( events
                |> getBaselineConcur index
                |> makeConceptsFilter cpt
                |> combineIntervals
                |> durations
                |> any (>= 90) )
            ||
            ( events
                |> getBaselineConcur index
                |> relations
                |> filter (== Equals)
                |> not . null) 
    )


{-------------------------------------------------------------------------------
  Features used by inclusion/exclusion (and possibly other places too)
-------------------------------------------------------------------------------}

-- | Lift a subject's events in a feature
featureEvents :: Events Day -> Feature "allEvents" (Events Day)
featureEvents = pure

-- | Lift a calendar index into a feature
featureIndex :: Index Interval Day -> Feature "calendarIndex"  (Index Interval Day)
featureIndex = pure

--- | Gets all enrollment intervals and combines them any place they concur. 
--    Returns an error if there are no enrollment intervals.
enrollmentIntervals :: Definition
  (  Feature "allEvents" (Events Day)
  -> Feature "enrollmentIntervals" [Interval Day])
enrollmentIntervals =
  defineA
      (\events ->
              events
          |> makeConceptsFilter ["enrollment"]
          |> combineIntervals
          |> (\x -> if null x then makeFeature $ missingBecause $ Other "no enrollment intervals"
                    else pure x)
      )

-- | The subject's age at time of index. Returns an error if there no birth year
--   records.
age :: Definition
  (   Feature "calendarIndex" (Index Interval Day)
   -> Feature "allEvents" (Events Day)
   -> Feature "age" Integer)
age =
  defineA
    (\index events ->
      events
      |> makeConceptsFilter ["is_birth_year"]
      |> viewBirthYears
      |> headMay
      |> fmap (\y  -> fromGregorian y 1 7)  -- Use July 1 YEAR as birthdate
      |> fmap (\bday -> computeAgeAt bday (begin $ getIndex index) )
      |> \case
            Nothing -> makeFeature $ featureDataL $ Other "No numeric birth year found"
            Just age -> pure age
    )

-- | Just the day of death (the first if there are multiple). Nothing if there
--   are no death records.
deathDay :: Definition
  (   Feature "allEvents" (Events Day)
   -> Feature "deathDay" (Maybe (Interval Day)))
deathDay =
  define
    (\events ->
           events
        |> makeConceptsFilter ["is_death"]
        |> intervals
        |> headMay
    )

{-------------------------------------------------------------------------------
  Inclusion/Exclusion features 
-------------------------------------------------------------------------------}

-- | Include the subject if female; Exclude otherwise
critFemale :: Definition
 (   Feature "allEvents" (Events Day)
  -> Feature "isFemale" Status)

critFemale =
    define
      (\events ->
            events
        |> makeConceptsFilter ["is_female"]
        |> headMay
        |> \case
            Nothing -> Exclude
            Just _  -> Include
      )

-- | Include the subject if over 50; Exclude otherwise.
critOver50 :: Definition
  (   Feature "age" Integer
   -> Feature "isOver50" Status)
critOver50 = define (includeIf . (>= 50))

-- | Include the subject if she has an enrollment interval concurring with index.
critEnrolled :: Definition
  (   Feature "calendarIndex" (Index Interval Day)
   -> Feature "enrollmentIntervals" [Interval Day]
   -> Feature "isEnrolled" Status )
critEnrolled =
  define
      (\index enrollmentIntervals ->
        enrollmentIntervals
        |> any (concur $ getIndex index)
        |> includeIf
      )

-- | Include the subject if both:
--     * she is enrolled on index ('critEnrolled')
--     * she all the gaps between the (combined) enrolled intervals within baseline 
--       are less than 30 days
critEnrolled455 :: Definition
  (   Feature "calendarIndex" (Index Interval Day)
   -> Feature "enrollmentIntervals" [Interval Day]
   -> Feature "isEnrolled" Status
   -> Feature "isContinuousEnrolled" Status )
critEnrolled455 =
  define
      (\index enrollIntrvls isEnrolled ->
        case isEnrolled of
          Exclude -> Exclude
          Include -> includeIf ( allGapsWithinLessThanDuration 30 (baselineInterval index) enrollIntrvls)
      )

-- | Exclude if the subject is dead before the time of index.
critDead :: Definition
  (  Feature "calendarIndex" (Index Interval Day)
  -> Feature "deathDay" (Maybe (Interval Day))
  -> Feature "isDead" Status)
critDead =
  define
      (\index mDeadDay ->
          case mDeadDay of
            Nothing -> Include
            Just deadDay  -> excludeIf $ beforeIndex index deadDay
        --  excludeIf ( maybe False (beforeIndex index) mDeadDay) -- different way to write logic
      )



{-------------------------------------------------------------------------------
  Covariate features
-------------------------------------------------------------------------------}

type BoolFeatDef n =
    Definition
    (    Feature "calendarIndex" (Index Interval Day)
      -> Feature "allEvents" (Events Day)
      -> Feature n Bool
    )

type BoolFeat n = Feature n  Bool

diabetes :: BoolFeatDef "diabetes"
diabetes = twoOutOneIn ["is_diabetes_outpatient"] ["is_diabetes_inpatient"]

ckd :: BoolFeatDef "ckd"
ckd =  twoOutOneIn ["is_ckd_outpatient"] ["is_ckd_inpatient"]

ppi :: BoolFeatDef "ppi"
ppi = medHx ["is_ppi"]

glucocorticoids :: BoolFeatDef "glucocorticoids"
glucocorticoids = medHx ["is_glucocorticoids"]

{-------------------------------------------------------------------------------
  Cohort Specifications and evaluation
-------------------------------------------------------------------------------}



-- | Make a function that runs the criteria for a calendar index
makeCriteriaRunner :: Index Interval Day -> Events Day -> Criteria
makeCriteriaRunner index events =
  criteria $
      criterion crit1 :| -- Note use of NonEmpty constructor
    [ criterion crit2
    , criterion crit3
    , criterion crit4
    , criterion crit5 ]
  where crit1   = eval critFemale featEvs
        crit2   = eval critOver50 agefeat
        crit3   = eval critEnrolled (featInd, enrll)
        crit4   = eval critEnrolled455 (featInd, enrll, crit3)
        crit5   = eval critDead (featInd, dead)
        agefeat = eval age (featInd, featEvs)
        enrll   = eval enrollmentIntervals featEvs
        dead    = eval deathDay featEvs
        featInd = featureIndex index
        featEvs = featureEvents events

-- | Define the shape of features for a cohort
type ExampleFeatures =
    ( Feature "calendarIndex"  (Index Interval Day)
    , BoolFeat "diabetes"
    , BoolFeat "ckd"
    , BoolFeat "ppi"
    , BoolFeat "glucocorticoids"
    )

-- | Make a function that runs the features for a calendar index
makeFeatureRunner ::
       Index Interval Day
    -> Events Day
    -> ExampleFeatures
makeFeatureRunner index events = (
      idx
    , eval diabetes (idx,  ef)
    , eval ckd (idx,  ef)
    , eval ppi (idx,  ef)
    , eval glucocorticoids (idx, ef)
    )
    where idx = featureIndex index
          ef  = featureEvents events

-- | Make a cohort specification for each calendar time
cohortSpecs :: [CohortSpec (Events Day) ExampleFeatures]
cohortSpecs =
  map (\x -> specifyCohort (makeCriteriaRunner x) (makeFeatureRunner x))
  indices

-- | A function that evaluates all the calendar cohorts for a population
evalCohorts :: Population (Events Day) -> [Cohort ExampleFeatures]
evalCohorts pop = map (`evalCohort` pop) cohortSpecs

{-------------------------------------------------------------------------------
  Testing 
  This would generally be in a separate file
-------------------------------------------------------------------------------}
m :: Year -> MonthOfYear -> Int -> Integer -> [Text] -> Domain -> Event Day
m y m d dur c dmn = event (beginerval dur (fromGregorian y m d)) (context dmn (packConcepts c))

testData1 :: Events Day
testData1 = sort
    [ m 2010 1 1 1   ["is_female"] (Demographics (DemographicsFacts (DemographicsInfo Gender  (Just "Female")) ))
    , m 2010 1 1 1   ["is_birth_year"] (Demographics (DemographicsFacts (DemographicsInfo BirthYear (Just "1960")) ))
    , m 2016 1 1 699 ["enrollment"] ( UnimplementedDomain ())
    , m 2018 1 1 30  ["enrollment"] ( UnimplementedDomain ())
    , m 2018 2 1 30  ["enrollment"] ( UnimplementedDomain ())
    , m 2017 6 5 1   ["is_diabetes_inpatient"] (UnimplementedDomain ())
    , m 2017 8 1 91  ["is_ppi"] (UnimplementedDomain ())
    ]

testSubject1 :: Subject (Events Day)
testSubject1 = MkSubject ("a", testData1)

testData2 :: Events Day
testData2 = sort
    [ m 2010 1 1 1   ["is_female"] (Demographics (DemographicsFacts (DemographicsInfo Gender  (Just "Female")) ))
    , m 2010 1 1 1   ["is_birth_year"] (Demographics (DemographicsFacts (DemographicsInfo BirthYear (Just "1980")) ))
    , m 2016 1 1 730 ["enrollment"] (UnimplementedDomain ())
    , m 2018 1 1 30  ["enrollment"] (UnimplementedDomain ())
    , m 2018 2 1 30  ["enrollment"] (UnimplementedDomain ())
    ]

testSubject2 :: Subject (Events Day)
testSubject2 = MkSubject ("b", testData2)

testPop :: Population (Events Day)
testPop = MkPopulation [testSubject1, testSubject2]

makeExpectedCovariate :: (KnownSymbol name) => FeatureData Bool -> Feature name  Bool
makeExpectedCovariate = makeFeature

makeExpectedFeatures ::
  FeatureData (Index Interval Day)
  -> (FeatureData Bool, FeatureData Bool, FeatureData Bool, FeatureData Bool)
  -> ExampleFeatures
makeExpectedFeatures i (b1, b2, b3, b4) =
        ( makeFeature  i :: Feature "calendarIndex"  (Index Interval Day)
        , makeExpectedCovariate b1
        , makeExpectedCovariate b2
        , makeExpectedCovariate b3
        , makeExpectedCovariate b4
        )

expectedFeatures1 :: [ExampleFeatures]
expectedFeatures1 =
  map (uncurry makeExpectedFeatures)
    [ (pure $ makeIndex $ beginerval 1 (fromGregorian 2017 4 1),
          (pure False, pure False, pure False, pure False))
    , (pure $ makeIndex $ beginerval 1 (fromGregorian 2017 7 1),
           (pure True, pure False, pure False, pure False))
    , (pure $ makeIndex $ beginerval 1 (fromGregorian 2017 10 1),
           (pure True, pure False, pure True, pure False))
    ]

expectedObsUnita :: [ObsUnit ExampleFeatures]
expectedObsUnita = zipWith (curry MkObsUnit) (replicate 5 "a") expectedFeatures1

makeExpectedCohort :: AttritionInfo -> [ObsUnit ExampleFeatures] -> Cohort ExampleFeatures
makeExpectedCohort a x = MkCohort (a, x)

expectedCohorts :: [Cohort ExampleFeatures]
expectedCohorts =
  zipWith
  (curry MkCohort)
  [
    MkAttritionInfo [(ExcludedBy (2, "isOver50"), 1), (ExcludedBy (4, "isContinuousEnrolled"), 1)]
  , MkAttritionInfo [(ExcludedBy (2, "isOver50"), 1), (Included, 1)]
  , MkAttritionInfo [(ExcludedBy (2, "isOver50"), 1), (Included, 1)]
  , MkAttritionInfo [(ExcludedBy (2, "isOver50"), 1), (Included, 1)]
  , MkAttritionInfo [(ExcludedBy (2, "isOver50"), 1), (ExcludedBy (4, "isContinuousEnrolled"), 1)]
  , MkAttritionInfo [(ExcludedBy (2, "isOver50"), 1), (ExcludedBy (3, "isEnrolled"), 1)]
  , MkAttritionInfo [(ExcludedBy (2, "isOver50"), 1), (ExcludedBy (3, "isEnrolled"), 1)]
  , MkAttritionInfo [(ExcludedBy (2, "isOver50"), 1), (ExcludedBy (3, "isEnrolled"), 1)]
  ]
  ([[]] ++ transpose [expectedObsUnita] ++ [[], [], [], []])

exampleCohort1tests :: TestTree
exampleCohort1tests = testGroup "Unit tests for calendar cohorts"
  [ testCase "expected Features for testData1" $
       evalCohorts testPop @?= expectedCohorts
  ]
