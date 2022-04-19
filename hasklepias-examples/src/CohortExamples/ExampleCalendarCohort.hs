{-|
Description : Demostrates how to define a calendar cohort using Hasklepias
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module CohortExamples.ExampleCalendarCohort where

import           ExampleEvents                  ( Demographic(..)
                                                , ExampleModel(..)
                                                )
import           Hasklepias

{-------------------------------------------------------------------------------
Define input data type
-------------------------------------------------------------------------------}
{- tag::projectEvent[] -}
type Evnt a = Event Text ExampleModel a
{- end::projectEvent[] -}

{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

-- | Calendar indices: first day of each quarter for 2017
{- tag::indices[] -}
indices :: [Interval Day]
indices = map (\(y, m) -> beginervalMoment (fromGregorian y m 1))
              (allPairs [2017] [1, 4, 7, 10])
{- end::indices[] -}

{-------------------------------------------------------------------------------
  Inclusion/Exclusion features 
-------------------------------------------------------------------------------}

{- tag::enrollPredicate[] -}
isEnrollmentEvent :: Predicate (Evnt Day)
isEnrollmentEvent = Predicate
  (\x -> case getFacts (getContext x) of
    Enrollment -> True
    _          -> False
  )
{- end::enrollPredicate[] -}

-- | Include the subject if she has an enrollment interval concurring with index
{- tag::enrolled[] -}
critEnrolled
  :: Def
       (  F "calendarIndex" (Interval Day)
       -> F "allEvents" [Evnt Day]
       -> F "isEnrolled" Status
       )
critEnrolled = buildIsEnrolled isEnrollmentEvent
{- end::enrolled[] -}

-- | Include the subject if both:
--     * she is enrolled on index ('critEnrolled')
--     * she all the gaps between the (combined) enrolled intervals within baseline 
--       are less than 30 days
{- tag::contenrolled[] -}
critEnrolled455
  :: Def
       (  F "calendarIndex" (Interval Day)
       -> F "allEvents" [Evnt Day]
       -> F "isEnrolled" Status
       -> F "isContinuousEnrolled" Status
       )
critEnrolled455 =
  buildContinuousEnrollment (makeBaselineFromIndex 455) isEnrollmentEvent 30
{- end::contenrolled[] -}

{-------------------------------------------------------------------------------
  Cohort Specifications and evaluation
-------------------------------------------------------------------------------}

{- tag::indexrunner[] -}
makeIndexRunner :: Interval Day -> [Evnt Day] -> IndexSet (Interval Day)
makeIndexRunner i _ = makeIndexSet [i]
{- end::indexrunner[] -}


-- | Make a function that runs the criteria for a calendar index
{- tag::criteriarunner[] -}
makeCriteriaRunner :: Interval Day -> [Evnt Day] -> Criteria
makeCriteriaRunner index events = criteria [criterion crit1, criterion crit2]
 where
  crit1   = eval critEnrolled featInd featEvs
  crit2   = eval critEnrolled455 featInd featEvs crit1
  featInd = pure index
  featEvs = pure events
{- end::criteriarunner[] -}

-- | Make a cohort specification for each calendar time
cohortSpecs :: CohortMapSpec [Evnt Day] () (Interval Day)
cohortSpecs = makeCohortSpecs $ map
  (\x -> (pack $ show x, makeIndexRunner x, makeCriteriaRunner, noFeatures))
  indices
  where noFeatures i e = ()

-- | A function that evaluates all the calendar cohorts for a population
evalCohorts
  :: Monad m => Population [Evnt Day] -> m (CohortMap () (Interval Day))
evalCohorts = makeCohortSpecsEvaluator defaultCohortEvalOptions cohortSpecs

{-------------------------------------------------------------------------------
  Testing
  This would generally be in a separate file
-------------------------------------------------------------------------------}
m
  :: Year -> MonthOfYear -> Int -> Integer -> [Text] -> ExampleModel -> Evnt Day
m y m d dur c dmn = event itv ctx where
  ctx = context (packConcepts c) dmn Nothing
  itv = beginerval dur (fromGregorian y m d)

testData1 :: [Evnt Day]
testData1 = sort
  [ m 2010 1 1 1   ["is_female"]     (Demographics (Gender "Female"))
  , m 2010 1 1 1   ["is_birth_year"] (Demographics (BirthYear 1960))
  , m 2016 1 1 699 ["enrollment"]    Enrollment
  , m 2018 1 1 30  ["enrollment"]    Enrollment
  , m 2018 2 1 30  ["enrollment"]    Enrollment
  ]

testSubject1 :: Subject [Evnt Day]
testSubject1 = into ("a" :: Text, testData1)

testData2 :: [Evnt Day]
testData2 = sort
  [ m 2010 1 1 1   ["is_female"]     (Demographics (Gender "Female"))
  , m 2010 1 1 1   ["is_birth_year"] (Demographics (BirthYear 1980))
  , m 2015 1 1 730 ["enrollment"]    Enrollment
  ]

testSubject2 :: Subject [Evnt Day]
testSubject2 = into ("b" :: Text, testData2)

testPop :: Population [Evnt Day]
testPop = into [testSubject1, testSubject2]

expectedObsUnita :: [ObsUnit () (Interval Day)]
expectedObsUnita = map from pairs where
  pairs = zip
    (fmap
      (\(x, y) -> makeObsID (beginerval 1 (fromGregorian x y 1)) ("a" :: Text))
      [(2017, 4), (2017, 7), (2017, 10)]
    )
    [(), (), ()]

makeExpectedCohort
  :: AttritionInfo -> [ObsUnit ExampleModel ()] -> Cohort ExampleModel ()
makeExpectedCohort a x = MkCohort (a, into x)

expectedCohorts :: [Cohort () (Interval Day)]
expectedCohorts = zipWith
  (curry MkCohort)
  [ makeTestAttritionInfo
    2
    2
    [ (SubjectHasNoIndex                     , 0)
    , (ExcludedBy (1, "isEnrolled")          , 1)
    , (ExcludedBy (2, "isContinuousEnrolled"), 1)
    , (Included                              , 0)
    ]
  , makeTestAttritionInfo
    2
    2
    [ (SubjectHasNoIndex                     , 0)
    , (ExcludedBy (1, "isEnrolled")          , 1)
    , (ExcludedBy (2, "isContinuousEnrolled"), 0)
    , (Included                              , 1)
    ]
  , makeTestAttritionInfo
    2
    2
    [ (SubjectHasNoIndex                     , 0)
    , (ExcludedBy (1, "isEnrolled")          , 1)
    , (ExcludedBy (2, "isContinuousEnrolled"), 0)
    , (Included                              , 1)
    ]
  , makeTestAttritionInfo
    2
    2
    [ (SubjectHasNoIndex                     , 0)
    , (ExcludedBy (1, "isEnrolled")          , 1)
    , (ExcludedBy (2, "isContinuousEnrolled"), 0)
    , (Included                              , 1)
    ]
  ]
  (fmap into ([] : transpose [expectedObsUnita]))

expectedCohortSet :: CohortMap () (Interval Day)
expectedCohortSet = into $ into @(Map Text (Cohort () (Interval Day))) $ zip
  (fmap (pack . show) indices)
  expectedCohorts

example :: TestTree
example = testGroup
  "Unit tests for calendar cohorts"
  [ testCase "expected Features for testData1"
    $ -- Featureable cannot be tested for equality directly, hence encoding to
      -- JSON bytestring and testing that for equality
        encode (evalCohorts @[] testPop)
    @?= encode [expectedCohortSet]
  ]
