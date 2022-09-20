{-|
Description : Demostrates how to define a calendar cohort using Hasklepias
-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module CohortExamples.CalendarCohort where

import           ExampleEvents (Demographic (..), ExampleModel (..))
import           Hasklepias

{-------------------------------------------------------------------------------
Define input data type
-------------------------------------------------------------------------------}
{- tag::input[] -}
type Evnt = Event Text ExampleModel Day
{- end::input[] -}


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
isEnrollmentEvent :: Predicate Evnt
isEnrollmentEvent = Predicate
  (\x -> case getFacts (getContext x) of
    Enrollment -> True
    _          -> False
  )
{- end::enrollPredicate[] -}

-- | Include the subject if they have an enrollment interval
-- concurring with index
{- tag::enrolled[] -}
enrolled :: Interval Day -> [Evnt] -> Status
enrolled i es = includeIf . not . null $ filterEvents
  (Predicate (concur i) &&& isEnrollmentEvent)
  es
{- end::enrolled[] -}


{-------------------------------------------------------------------------------
  Cohort Specifications and evaluation
-------------------------------------------------------------------------------}

{- tag::indexrunner[] -}
makeIndexRunner :: Interval Day -> [Evnt] -> IndexSet (Interval Day)
makeIndexRunner i _ = makeIndexSet [i]
{- end::indexrunner[] -}

-- | Make a function that runs the criteria for a calendar index
{- tag::criteriarunner[] -}
makeCriteriaRunner :: Interval Day -> [Evnt] -> Criteria
makeCriteriaRunner index events = into @Criteria
  [makeCriterion "isEnrolled" crit1]
  where crit1 = enrolled index events
{- end::criteriarunner[] -}

-- | Make a cohort specification for each calendar time
specs :: CohortMapSpec [Evnt] () (Interval Day)
specs = makeCohortSpecs $ map
  (\x -> (pack $ show x, makeIndexRunner x, makeCriteriaRunner, noFeatures))
  indices
  where noFeatures i e = ()

-- | A function that evaluates all the calendar cohorts for a population
evalCohorts :: Monad m => Population [Evnt] -> m (CohortMap () (Interval Day))
evalCohorts = makeCohortSpecsEvaluator defaultCohortEvalOptions specs

{-------------------------------------------------------------------------------
  Testing
  This would generally be in a separate file
-------------------------------------------------------------------------------}
m :: Year -> MonthOfYear -> Int -> Integer -> [Text] -> ExampleModel -> Evnt
m y m d dur t dmn = event itv ctx where
  ctx = context (packTagSet t) dmn Nothing
  itv = beginerval dur (fromGregorian y m d)

testData1 :: [Evnt]
testData1 = sort
  [ m 2010 1 1 1   ["is_female"]     (Demographics (Gender "Female"))
  , m 2010 1 1 1   ["is_birth_year"] (Demographics (BirthYear 1960))
  , m 2017 1 2 365 []                Enrollment
  ]

testSubject1 :: Subject [Evnt]
testSubject1 = into ("a" :: Text, testData1)

testData2 :: [Evnt]
testData2 = sort
  [ m 2010 1 1 1   ["is_female"]     (Demographics (Gender "Female"))
  , m 2010 1 1 1   ["is_birth_year"] (Demographics (BirthYear 1980))
  , m 2015 1 1 730 []                Enrollment
  ]

testSubject2 :: Subject [Evnt]
testSubject2 = into ("b" :: Text, testData2)

testPop :: Population [Evnt]
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
    [(SubjectHasNoIndex, 0), (ExcludedBy (1, "isEnrolled"), 2), (Included, 0)]
  , makeTestAttritionInfo
    2
    2
    [(SubjectHasNoIndex, 0), (ExcludedBy (1, "isEnrolled"), 1), (Included, 1)]
  , makeTestAttritionInfo
    2
    2
    [(SubjectHasNoIndex, 0), (ExcludedBy (1, "isEnrolled"), 1), (Included, 1)]
  , makeTestAttritionInfo
    2
    2
    [(SubjectHasNoIndex, 0), (ExcludedBy (1, "isEnrolled"), 1), (Included, 1)]
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
