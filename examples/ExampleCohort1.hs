{-# LANGUAGE LambdaCase #-}
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
module ExampleCohort1(
  exampleCohort1tests
--     exampleCohort1Spec
) where

import Hasklepias
{-------------------------------------------------------------------------------

  Define the desired Feature Attributes.
  
  TODO: In future versions the basic attributes will be defined in hasklepias or 
  some other library.
-------------------------------------------------------------------------------}
data Attributes = MkAttributes { getLabel :: Text, getPurpose :: [Purpose]}
    deriving (Eq, Show, Generic)

data Purpose =
      Outcome
    | Covariate
    | Intermediate
    | Criteria
    deriving (Eq, Show, Generic)

instance ToJSON Purpose where
instance ToJSON Attributes where

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

-- | A helper function to specify a @Feature@.
makeExampleFeatureSpec ::
           Text -- ^ machine readable name of feature
        -> Text -- ^ label
        -> [Purpose] -- ^ purposes
        -> FeatureDefinition di d
        -> FeatureSpec Attributes di d
makeExampleFeatureSpec name label purposes =
    specifyFeature name (MkAttributes label purposes)

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
    -> FeatureDefinition (FeatureData (Index Interval Day), FeatureData (Events Day)) Bool
twoOutOneIn cpts1 cpts2 = define2
    (\index events ->
        atleastNofX 1 cpts1  (getBaselineConcur index events) ||
        (anyGapsWithinAtLeastDuration 7 (baselineInterval index) . makeConceptsFilter cpts2) events
    )

-- | Defines a feature that returns 'True' ('False' otherwise) if either:
--   * any events concuring with baseline with concepts in 'cpts' have a 
--     duration >= 90 days
--   * at least 2 events with concepts in 'cpts' have the same interval 
medHx :: [Text]
      -> FeatureDefinition (FeatureData (Index Interval Day), FeatureData (Events Day)) Bool
medHx cpt = define2
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
                |> not . null )
    )


{-------------------------------------------------------------------------------
  Inclusion/Exclusion features 
-------------------------------------------------------------------------------}

-- | Include the subject if female; Exclude otherwise
critFemale :: FeatureSpec Attributes (FeatureData (Events Day)) Status
critFemale =
    makeExampleFeatureSpec
        "isFemale"
        "Is this subject female?"
        [Intermediate]
        ( define
          (\events ->
               events
            |> makeConceptsFilter ["is_female"]
            |> headMay
            |> \case
                Nothing -> Exclude
                Just _  -> Include
          )
        )

-- | Include the subject if over 50; Exclude otherwise.
critOver50 :: FeatureSpec Attributes (FeatureData Integer) Status
critOver50 =
    makeExampleFeatureSpec
        "isOver50"
        "Is this subject over 50 at time of index?"
        [Intermediate]
        ( define (includeIf . (>= 50)))

-- | Include the subject if she has an enrollment interval concurring with index.
critEnrolled ::
    FeatureSpec
        Attributes
        (FeatureData (Index Interval Day), FeatureData [Interval Day])
        Status
critEnrolled =
    makeExampleFeatureSpec
        "isEnrolled"
        "Is this subject enrolled at time of index?"
        [Intermediate]
        ( define2
            (\index enrollmentIntervals ->
              enrollmentIntervals
              |> any (concur $ getIndex index)
              |> includeIf
            )
        )

-- | Include the subject if both:
--     * she is enrolled on index ('critEnrolled')
--     * she all the gaps between the (combined) enrolled intervals within baseline 
--       are less than 30 days
critEnrolled455 ::
    FeatureSpec
        Attributes
        (FeatureData (Index Interval Day), FeatureData [Interval Day], FeatureData Status)
        Status
critEnrolled455 =
    makeExampleFeatureSpec
        "isContinuousEnrolled"
        "Is this subject continuously enrolled for 455 days prior to index?"
        [Intermediate]
        ( define3
            (\index enrollIntrvls isEnrolled ->
              case isEnrolled of
                Exclude -> Exclude
                Include -> includeIf ( allGapsWithinLessThanDuration 30 (baselineInterval index) enrollIntrvls)
            )
        )

-- | Exclude if the subject is dead before the time of index.
critDead ::
    FeatureSpec
        Attributes
        (FeatureData (Index Interval Day), FeatureData (Maybe (Interval Day)))
        Status
critDead =
    makeExampleFeatureSpec
        "isDead"
        "Is this subject dead at time of index?"
        [Intermediate]
        ( define2
           (\index mDeadDay ->
               case mDeadDay of
                 Nothing -> Include
                 Just deadDay  -> excludeIf $ beforeIndex index deadDay
              --  excludeIf ( maybe False (beforeIndex index) mDeadDay) -- different way to write logic
           )
        )

{-------------------------------------------------------------------------------
  Features used by inclusion/exclusion (and possibly other places too)
-------------------------------------------------------------------------------}

--- | Gets all enrollment intervals and combines them any place they concur. 
--    Returns an error if there are no enrollment intervals.
enrollmentIntervals :: FeatureSpec Attributes (FeatureData (Events Day)) [Interval Day]
enrollmentIntervals =
    makeExampleFeatureSpec
        "enrollmentIntervals"
        "All (combined) enrollment intervals"
        [Intermediate]
        ( defineM
            (\events ->
                   events
                |> makeConceptsFilter ["enrollment"]
                |> combineIntervals
                |> (\x -> if null x then featureDataL $ Other "no enrollment intervals"
                          else pure x)
            )

        )

-- | The subject's age at time of index. Returns an error if there no birth year
--   records.
age ::
  FeatureSpec
      Attributes
      (FeatureData (Index Interval Day), FeatureData (Events Day))
      Integer
age =
    makeExampleFeatureSpec
        "age"
        "Subject's age index"
        [Covariate, Intermediate]
        ( defineM2
          (\index events ->
            events
            |> makeConceptsFilter ["is_birth_year"]
            |> viewBirthYears
            |> headMay
            |> fmap (\y  -> fromGregorian y 1 7)  -- Use July 1 YEAR as birthdate
            |> fmap (\bday -> computeAgeAt bday (begin $ getIndex index) )
            |> \case
                  Nothing -> featureDataL $ Other "No numeric birth year found"
                  Just age -> featureDataR age
          )

        )

-- | Just the day of death (the first if there are multiple). Nothing if there
--   are no death records.
deathDay ::
    FeatureSpec
        Attributes
        (FeatureData (Events Day))
        (Maybe (Interval Day))
deathDay =
    makeExampleFeatureSpec
        "deathDay"
        "Day of Death (Nothing if no death records)"
        [Intermediate]
        ( defineM
            (\events ->
                   events
                |> makeConceptsFilter ["is_death"]
                |> intervals
                |> headMay
                |> pure
            )
        )

{-------------------------------------------------------------------------------
  Covariate features
-------------------------------------------------------------------------------}

-- | Creates features for all comorbidity indicators 
comorbidities :: [FeatureSpec Attributes (FeatureData (Index Interval Day), FeatureData (Events Day)) Bool]
comorbidities = map (\(n, l, p, args) -> makeExampleFeatureSpec n l p (uncurry twoOutOneIn args))
    [  ("diabetes", "", [Covariate], ( ["is_diabetes_outpatient"], ["is_diabetes_inpatient"] ))
    ,  ("ckd"     , "", [Covariate], ( ["is_ckd_outpatient"], ["is_ckd_inpatient"]))
    ]

-- | Creates features for all medication indicators 
medicationHx :: [FeatureSpec Attributes (FeatureData (Index Interval Day), FeatureData (Events Day)) Bool]
medicationHx = map (\(n, l, p, args) -> makeExampleFeatureSpec n l p (medHx args))
    [  ("ppi", "", [Covariate], ["is_ppi"])
    ,  ("glucocorticoids"     , "", [Covariate], ["is_glucocorticoids"])
    ]

{-------------------------------------------------------------------------------
  Cohort Specifications and evaluation
-------------------------------------------------------------------------------}

-- | Lift a subject's events in a feature
featureEvents :: Events Day -> Feature Attributes (Events Day)
featureEvents x=  MkFeature "allEvents" (MkAttributes "" [Intermediate])
                    (pure x)

-- | Lift an index into a feature
featureIndex :: Index Interval Day -> Feature Attributes (Index Interval Day)
featureIndex x = MkFeature "calendarIndex" (MkAttributes "" [Intermediate])
                    (pure x)

-- | Make a function that runs the criteria for a calendar index
makeCriteriaRunner :: Index Interval Day -> Events Day -> Criteria Attributes
makeCriteriaRunner index events =
  criteria $
      criterion crit1 :| -- Note use of NonEmpty constructor
    [ criterion crit2
    , criterion crit3
    , criterion crit4
    , criterion crit5 ]
  where crit1   = evalSpec critFemale featEvs
        crit2   = evalSpec critOver50 agefeat
        crit3   = evalSpec critEnrolled (featInd, enrll)
        crit4   = evalSpec critEnrolled455 (featInd, enrll, getData crit3)
        crit5   = evalSpec critDead (featInd, dead)
        agefeat = getData $ evalSpec age (featInd, featEvs)
        enrll   = getData $ evalSpec enrollmentIntervals featEvs
        dead    = getData $ evalSpec deathDay featEvs
        featInd = getData $ featureIndex index
        featEvs = getData $ featureEvents events

-- | Define the shape of features for a cohort
type ExampleFeatures =
    ( Feature Attributes  (Index Interval Day) -- the index
    , [Feature Attributes Bool] -- covariates
    )

-- | Make a function that runs the features for a calendar index
makeFeatureRunner ::
       Index Interval Day
    -> Events Day
    -> ExampleFeatures
makeFeatureRunner index events = (
      idx
    , map (\spec -> evalSpec spec (getData idx, getData ef)) comorbidities ++
      map (\spec -> evalSpec spec (getData idx, getData ef)) medicationHx
    )
    where idx = featureIndex index
          ef  = featureEvents events

-- | Make a cohort specification for each calendar time
cohortSpecs :: [CohortSpec Attributes (Events Day) ExampleFeatures]
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
m :: Year -> MonthOfYear -> Int -> Integer -> [Text] -> Maybe Domain -> Event Day
m y m d dur c dmn = event (beginerval dur (fromGregorian y m d)) (context dmn (packConcepts c))

testData1 :: Events Day
testData1 = sort
    [ m 2010 1 1 1   ["is_female"] (Just (Demographics (DemographicsFacts (DemographicsInfo Gender  (Just "Female")) )))
    , m 2010 1 1 1   ["is_birth_year"] (Just (Demographics (DemographicsFacts (DemographicsInfo BirthYear (Just "1960")) )))
    , m 2016 1 1 699 ["enrollment"] (Just (UnimplementedDomain ()))
    , m 2018 1 1 30  ["enrollment"] (Just (UnimplementedDomain ()))
    , m 2018 2 1 30  ["enrollment"] (Just (UnimplementedDomain ()))
    , m 2017 6 5 1   ["is_diabetes_inpatient"] (Just (UnimplementedDomain ()))
    , m 2017 8 1 91  ["is_ppi"] (Just (UnimplementedDomain ()))
    ]

testSubject1 :: Subject (Events Day)
testSubject1 = MkSubject ("a", testData1)

testData2 :: Events Day
testData2 = sort
    [ m 2010 1 1 1   ["is_female"] (Just (Demographics (DemographicsFacts (DemographicsInfo Gender  (Just "Female")) )))
    , m 2010 1 1 1   ["is_birth_year"] (Just (Demographics (DemographicsFacts (DemographicsInfo BirthYear (Just "1980")) )))
    , m 2016 1 1 730 ["enrollment"] (Just (UnimplementedDomain ()))
    , m 2018 1 1 30  ["enrollment"] (Just (UnimplementedDomain ()))
    , m 2018 2 1 30  ["enrollment"] (Just (UnimplementedDomain ()))
    ]

testSubject2 :: Subject (Events Day)
testSubject2 = MkSubject ("b", testData2)

testPop :: Population (Events Day)
testPop = MkPopulation [testSubject1, testSubject2]

makeExpectedFeatures ::
  FeatureData (Index Interval Day) -> [FeatureData Bool] -> ExampleFeatures
makeExpectedFeatures i bools =
        ( MkFeature "calendarIndex" (MkAttributes "" [Intermediate]) i
        , zipWith
            (\n b -> MkFeature n (MkAttributes "" [Covariate]) b)
            ["diabetes", "ckd", "ppi", "glucocorticoids"]
            bools
        )

expectedFeatures1 :: [ExampleFeatures]
expectedFeatures1 =
  map (uncurry makeExpectedFeatures)
    [ (pure $ makeIndex $ beginerval 1 (fromGregorian 2017 4 1),  map pure [False, False, False, False])
    , (pure $ makeIndex $ beginerval 1 (fromGregorian 2017 7 1),  map pure [True, False, False, False])
    , (pure $ makeIndex $ beginerval 1 (fromGregorian 2017 10 1), map pure [True, False, True, False])
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
