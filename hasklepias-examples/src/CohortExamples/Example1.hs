{-|
Description : Demostrates how to define a cohort using Hasklepias
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module CohortExamples.Example1
  ( example
  ) where

import           ExampleEvents                  ( Demographic(..)
                                                , ExampleModel(..)
                                                )
import           Hasklepias
{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

-- | Lookback duration for baseline
lookback455 :: Integer
lookback455 = 455

-- | Duration of follow up in months
followupDuration :: CalendarDiffDays
followupDuration = CalendarDiffDays 3 0

-- | Calendar indices: first day of each quarter for 2017
indices :: [Interval Day]
indices = map (\(y, m) -> beginerval 0 (fromGregorian y m 1))
              (allPairs [2017] [1, 4, 7, 10])

{-------------------------------------------------------------------------------
  Utility functions 
-------------------------------------------------------------------------------}

-- | Creates a baseline interval from index
baselineInterval :: Interval Day -> AssessmentInterval Day
baselineInterval = makeBaselineFromIndex lookback455

-- | Shifts an interval by a calendar amount
shiftIntervalDay
  :: (Intervallic i Day) => CalendarDiffDays -> i Day -> Interval Day
shiftIntervalDay cd i =
  beginerval (duration i) (addGregorianDurationClip cd (begin i))

-- | Creates an interval *beginning the same day as the index* and 
--   ending 'followupDuration' days later.
followupInterval :: Interval Day -> Interval Day
followupInterval index = beginerval
  (diff (begin index) (end $ shiftIntervalDay followupDuration index))
  (begin index)


-- | A predicate function that determines if some interval is before index
beforeIndex :: Intervallic i Day => Interval Day -> i Day -> Bool
beforeIndex = before

-- | Creates a filter for events to those that 'concur' with the baseline interval.
getBaselineConcur
  :: Interval Day
  -> [Event Text ExampleModel Day]
  -> [Event Text ExampleModel Day]
getBaselineConcur index = filterConcur (baselineInterval index)

{-------------------------------------------------------------------------------
  Feature patterns: functions for defining features by a given pattern 
-------------------------------------------------------------------------------}

-- | Defines a feature that returns 'True' ('False' otherwise) if either:
--   * at least 1 event during the baseline interval has any of the 'cpts1' concepts
--   * there are at least 2 events that have 'cpts2' concepts which have at least
--     7 days between them during the baseline interval
twoOutOneIn
  :: [Text] -- ^ cpts1
  -> [Text] -- ^ cpts2
  -> Definition
       (  Feature "calendarIndex" (Interval Day)
       -> Feature "allEvents" [Event Text ExampleModel Day]
       -> Feature name Bool
       )
twoOutOneIn cpts1 cpts2 = buildNofXOrMofYWithGapBool 1
                                                     (containsConcepts cpts1)
                                                     1
                                                     7
                                                     baselineInterval
                                                     concur
                                                     (containsConcepts cpts2)

-- | Defines a feature that returns 'True' ('False' otherwise) if either:
--   * any events concuring with baseline with concepts in 'cpts' have a 
--     duration >= 90 days
--   * at least 2 events with concepts in 'cpts' have the same interval 
medHx
  :: [Text]
  -> Definition
       (  Feature "calendarIndex" (Interval Day)
       -> Feature "allEvents" [Event Text ExampleModel Day]
       -> Feature name Bool
       )
medHx cpt = define
  (\index events ->
    (  events
      |> getBaselineConcur index
      |> filterEvents (containsConcepts cpt)
      |> combineIntervals
      |> durations
      |> any (>= 90)
      )
      || (  events
         |> getBaselineConcur index
         |> relationsL
         |> filter (== Equals)
         |> not
         .  null
         )
  )


{-------------------------------------------------------------------------------
  Features used by inclusion/exclusion (and possibly other places too)
-------------------------------------------------------------------------------}

-- | Lift a subject's events in a feature
featureEvents
  :: [Event Text ExampleModel Day]
  -> Feature "allEvents" [Event Text ExampleModel Day]
featureEvents = pure

-- | The subject's age at time of index. Returns an error if there no birth year
--   records.
age
  :: Definition
       (  Feature "calendarIndex" (Interval Day)
       -> Feature "allEvents" [Event Text ExampleModel Day]
       -> Feature "age" Integer
       )
age = defineA
  (\index events ->
    events
      |> filterEvents (containsConcepts ["is_birth_year"])
      |> fmap viewBirthYears
      |> catMaybes
      |> headMay
      |> fmap (\y -> fromGregorian y 1 7)  -- Use July 1 YEAR as birthdate
      |> fmap (\bday -> computeAgeAt bday (begin index))
      |> \case
           Nothing ->
             makeFeature $ featureDataL $ Other "No numeric birth year found"
           Just age -> pure age
  )
 where
  viewBirthYears :: Event Text ExampleModel Day -> Maybe Integer
  viewBirthYears x = case getFacts (getContext x) of
    Demographics (BirthYear v) -> Just v
    _                          -> Nothing


-- | Just the day of death (the first if there are multiple). Nothing if there
--   are no death records.
deathDay
  :: Definition
       (  Feature "allEvents" [Event Text ExampleModel Day]
       -> Feature "deathDay" (Maybe (Interval Day))
       )
deathDay = define
  (\events ->
    events
      |> filterEvents (containsConcepts ["is_death"])
      |> fmap getEvent
      |> intervals
      |> headMay
  )

{-------------------------------------------------------------------------------
  Inclusion/Exclusion features 
-------------------------------------------------------------------------------}

-- | Include the subject if female; Exclude otherwise
critFemale
  :: Definition
       (  Feature "allEvents" [Event Text ExampleModel Day]
       -> Feature "isFemale" Status
       )
critFemale = define
  (\events ->
    events |> filterEvents (containsConcepts ["is_female"]) |> headMay |> \case
      Nothing -> Exclude
      Just _  -> Include
  )

-- | Include the subject if over 50; Exclude otherwise.
critOver50 :: Definition (Feature "age" Integer -> Feature "isOver50" Status)
critOver50 = define (includeIf . (>= 50))

isEnrollmentEvent :: Predicate (Event Text ExampleModel Day)
isEnrollmentEvent = Predicate
  (\x -> case getFacts (getContext x) of
    Enrollment -> True
    _          -> False
  )

-- | Include the subject if she has an enrollment interval concurring with index.
critEnrolled
  :: Definition
       (  Feature "calendarIndex" (Interval Day)
       -> Feature "allEvents" [Event Text ExampleModel Day]
       -> Feature "isEnrolled" Status
       )
critEnrolled = buildIsEnrolled isEnrollmentEvent

-- | Include the subject if both:
--     * she is enrolled on index ('critEnrolled')
--     * she all the gaps between the (combined) enrolled intervals within baseline 
--       are less than 30 days
critEnrolled455
  :: Definition
       (  Feature "calendarIndex" (Interval Day)
       -> Feature "allEvents" [Event Text ExampleModel Day]
       -> Feature "isEnrolled" Status
       -> Feature "isContinuousEnrolled" Status
       )
critEnrolled455 =
  buildContinuousEnrollment baselineInterval isEnrollmentEvent 30

-- | Exclude if the subject is dead before the time of index.
critDead
  :: Definition
       (  Feature "calendarIndex" (Interval Day)
       -> Feature "deathDay" (Maybe (Interval Day))
       -> Feature "isDead" Status
       )
critDead = define
  (\index mDeadDay -> case mDeadDay of
    Nothing      -> Include
    Just deadDay -> excludeIf $ beforeIndex index deadDay
    --  excludeIf ( maybe False (beforeIndex index) mDeadDay) -- different way to write logic
  )

{-------------------------------------------------------------------------------
  Covariate features
-------------------------------------------------------------------------------}

type BoolFeatDef n
  = Definition
      (  Feature "calendarIndex" (Interval Day)
      -> Feature "allEvents" [Event Text ExampleModel Day]
      -> Feature n Bool
      )

type BoolFeat n = Feature n Bool

diabetes :: BoolFeatDef "diabetes"
diabetes = twoOutOneIn ["is_diabetes_inpatient"] ["is_diabetes_outpatient"]

instance HasAttributes  "diabetes" Bool where
  getAttributes _ = basicAttributes "Has Diabetes"
                                    "Has Diabetes within baseline"
                                    [Covariate]
                                    ["baseline"]


ckd :: BoolFeatDef "ckd"
ckd = twoOutOneIn ["is_ckd_inpatient"] ["is_ckd_outpatient"]

instance HasAttributes  "ckd" Bool where
  getAttributes _ =
    basicAttributes "Has ckd" "Has CKD within baseline" [Covariate] ["baseline"]

ppi :: BoolFeatDef "ppi"
ppi = medHx ["is_ppi"]

instance HasAttributes  "ppi" Bool where
  getAttributes _ =
    basicAttributes "Has ppi" "Has PPI within baseline" [Covariate] ["baseline"]

glucocorticoids :: BoolFeatDef "glucocorticoids"
glucocorticoids = medHx ["is_glucocorticoids"]

instance HasAttributes  "glucocorticoids" Bool where
  getAttributes _ = basicAttributes "Has glucocorticoids"
                                    "Has glucocorticoids within baseline"
                                    [Covariate]
                                    ["baseline"]

{-------------------------------------------------------------------------------
  Cohort Specifications and evaluation
-------------------------------------------------------------------------------}

makeIndexRunner
  :: Interval Day -> [Event Text ExampleModel Day] -> IndexSet (Interval Day)
makeIndexRunner i _ = makeIndexSet [i]

-- | Make a function that runs the criteria for a calendar index
makeCriteriaRunner :: Interval Day -> [Event Text ExampleModel Day] -> Criteria
makeCriteriaRunner index events =
  criteria
    $  criterion crit1
    :| -- Note use of NonEmpty constructor
       [criterion crit2, criterion crit3, criterion crit4, criterion crit5]
 where
  crit1   = eval critFemale featEvs
  crit2   = eval critOver50 agefeat
  crit3   = eval critEnrolled featInd featEvs
  crit4   = eval critEnrolled455 featInd featEvs crit3
  crit5   = eval critDead featInd dead
  agefeat = eval age featInd featEvs
  dead    = eval deathDay featEvs
  featInd = pure index
  featEvs = featureEvents events

instance HasAttributes "calendarIndex" (Interval Day) where

-- | Make a function that runs the features for a calendar index
makeFeatureRunner
  :: Interval Day -> [Event Text ExampleModel Day] -> Featureset
makeFeatureRunner index events = featureset
  (  packFeature idx
  :| [ packFeature $ eval diabetes idx ef
     , packFeature $ eval ckd idx ef
     , packFeature $ eval ppi idx ef
     , packFeature $ eval glucocorticoids idx ef
     ]
  )
 where
  idx = pure index
  ef  = featureEvents events

-- | Make a cohort specification for each calendar time
cohortSpecs
  :: CohortMapSpec [Event Text ExampleModel Day] Featureset (Interval Day)
cohortSpecs = makeCohortSpecs $ map
  (\x ->
    (pack $ show x, makeIndexRunner x, makeCriteriaRunner, makeFeatureRunner)
  )
  indices

-- | A function that evaluates all the calendar cohorts for a population
evalCohorts
  :: Monad m
  => Population [Event Text ExampleModel Day]
  -> m (CohortMap Featureset (Interval Day))
evalCohorts = makeCohortSpecsEvaluator defaultCohortEvalOptions cohortSpecs

{-------------------------------------------------------------------------------
  Testing
  This would generally be in a separate file
-------------------------------------------------------------------------------}
m
  :: Year
  -> MonthOfYear
  -> Int
  -> Integer
  -> [Text]
  -> ExampleModel
  -> Event Text ExampleModel Day
m y m d dur c dmn = event itv ctx where
  ctx = context (packConcepts c) dmn Nothing
  itv = beginerval dur (fromGregorian y m d)

testData1 :: [Event Text ExampleModel Day]
testData1 = sort
  [ m 2010 1 1 1   ["is_female"]             (Demographics (Gender "Female"))
  , m 2010 1 1 1   ["is_birth_year"]         (Demographics (BirthYear 1960))
  , m 2016 1 1 699 ["enrollment"]            Enrollment
  , m 2018 1 1 30  ["enrollment"]            Enrollment
  , m 2018 2 1 30  ["enrollment"]            Enrollment
  , m 2017 6 5 1   ["is_diabetes_inpatient"] Medical
  , m 2017 8 1 91  ["is_ppi"]                Medical
  ]

testSubject1 :: Subject [Event Text ExampleModel Day]
testSubject1 = into ("a" :: Text, testData1)

testData2 :: [Event Text ExampleModel Day]
testData2 = sort
  [ m 2010 1 1 1   ["is_female"]     (Demographics (Gender "Female"))
  , m 2010 1 1 1   ["is_birth_year"] (Demographics (BirthYear 1980))
  , m 2016 1 1 730 ["enrollment"]    Enrollment
  , m 2018 1 1 30  ["enrollment"]    Enrollment
  , m 2018 2 1 30  ["enrollment"]    Enrollment
  ]

testSubject2 :: Subject [Event Text ExampleModel Day]
testSubject2 = into ("b" :: Text, testData2)

testPop :: Population [Event Text ExampleModel Day]
testPop = into [testSubject1, testSubject2]

makeExpectedFeatures
  :: FeatureData (Interval Day)
  -> (FeatureData Bool, FeatureData Bool, FeatureData Bool, FeatureData Bool)
  -> Featureset
makeExpectedFeatures i (b1, b2, b3, b4) = featureset
  (  packFeature (makeFeature @"calendarIndex" i)
  :| [ packFeature (makeFeature @"diabetes" b1)
     , packFeature (makeFeature @"ckd" b2)
     , packFeature (makeFeature @"ppi" b3)
     , packFeature (makeFeature @"glucocorticoids" b4)
     ]
  )

expectedFeatures1 :: [Featureset]
expectedFeatures1 = map
  (uncurry makeExpectedFeatures)
  [ ( pure $ beginerval 1 (fromGregorian 2017 4 1)
    , (pure False, pure False, pure False, pure False)
    )
  , ( pure $ beginerval 1 (fromGregorian 2017 7 1)
    , (pure True, pure False, pure False, pure False)
    )
  , ( pure $ beginerval 1 (fromGregorian 2017 10 1)
    , (pure True, pure False, pure True, pure False)
    )
  ]

expectedObsUnita :: [ObsUnit Featureset (Interval Day)]
expectedObsUnita = map from pairs where
  pairs = zip
    (fmap
      (\(x, y) -> makeObsID (beginerval 1 (fromGregorian x y 1)) ("a" :: Text))
      [(2017, 4), (2017, 7), (2017, 10)]
    )
    expectedFeatures1

makeExpectedCohort
  :: AttritionInfo
  -> [ObsUnit ExampleModel Featureset]
  -> Cohort ExampleModel Featureset
makeExpectedCohort a x = MkCohort (a, into x)

expectedCohorts :: [Cohort Featureset (Interval Day)]
expectedCohorts = zipWith
  (curry MkCohort)
  [ makeTestAttritionInfo
    2
    2
    [ (SubjectHasNoIndex                     , 0)
    , (ExcludedBy (1, "isFemale")            , 0)
    , (ExcludedBy (2, "isOver50")            , 1)
    , (ExcludedBy (3, "isEnrolled")          , 0)
    , (ExcludedBy (4, "isContinuousEnrolled"), 1)
    , (ExcludedBy (5, "isDead")              , 0)
    , (Included                              , 0)
    ]
  , makeTestAttritionInfo
    2
    2
    [ (SubjectHasNoIndex                     , 0)
    , (ExcludedBy (1, "isFemale")            , 0)
    , (ExcludedBy (2, "isOver50")            , 1)
    , (ExcludedBy (3, "isEnrolled")          , 0)
    , (ExcludedBy (4, "isContinuousEnrolled"), 0)
    , (ExcludedBy (5, "isDead")              , 0)
    , (Included                              , 1)
    ]
  , makeTestAttritionInfo
    2
    2
    [ (SubjectHasNoIndex                     , 0)
    , (ExcludedBy (1, "isFemale")            , 0)
    , (ExcludedBy (2, "isOver50")            , 1)
    , (ExcludedBy (3, "isEnrolled")          , 0)
    , (ExcludedBy (4, "isContinuousEnrolled"), 0)
    , (ExcludedBy (5, "isDead")              , 0)
    , (Included                              , 1)
    ]
  , makeTestAttritionInfo
    2
    2
    [ (SubjectHasNoIndex                     , 0)
    , (ExcludedBy (1, "isFemale")            , 0)
    , (ExcludedBy (2, "isOver50")            , 1)
    , (ExcludedBy (3, "isEnrolled")          , 0)
    , (ExcludedBy (4, "isContinuousEnrolled"), 0)
    , (ExcludedBy (5, "isDead")              , 0)
    , (Included                              , 1)
    ]
  ]
  (fmap into ([] : transpose [expectedObsUnita]))

expectedCohortSet :: CohortMap Featureset (Interval Day)
expectedCohortSet =
  into $ mapFromList $ zip (fmap (pack . show) indices) expectedCohorts

example :: TestTree
example = testGroup
  "Unit tests for calendar cohorts"
  [ testCase "expected Features for testData1"
    $ -- Featureable cannot be tested for equality directly, hence encoding to
      -- JSON bytestring and testing that for equality
        encode (evalCohorts @[] testPop)
    @?= encode [expectedCohortSet]
  ]
