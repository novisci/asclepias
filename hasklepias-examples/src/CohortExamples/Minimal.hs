{-|
Description : Demostrates how to define a minimal cohort using Hasklepias
-}
{-# LANGUAGE DataKinds #-}
module CohortExamples.Minimal where

import           Hasklepias

{-------------------------------------------------------------------------------
  Cohort Specifications and evaluation
-------------------------------------------------------------------------------}


{-
The input day is index so we simply put in an IndexSet
-}
{- tag::indexrunner[] -}
runIndex :: (Day, Integer) -> IndexSet Day
runIndex (i, _) = makeIndexSet [i]
{- end::indexrunner[] -}

{-
We include subject's only if the day is after 2022-04-01
-}
{- tag::criteriarunner[] -}
runCriteria :: Day -> (Day, Integer) -> Criteria
runCriteria _ (d, _) = criteria [criterion "indexAfterAprilFools" crit1]
  where crit1 = includeIf (d > fromGregorian 2022 4 1)
{- end::criteriarunner[] -}

-- | Make a function that runs the criteria for a calendar index
{- tag::featurerunner[] -}
runOutput :: Day -> (Day, Integer) -> (Integer, Bool)
runOutput i (d, v) = (diffDays i d, v > 0)
{- end::criteriarunner[] -}

-- | Make a cohort specification
specs :: CohortMapSpec (Day, Integer) (Integer, Bool) Day
specs = makeCohortSpecs [("minimal-cohort", runIndex, runCriteria, runOutput)]

-- | A function that evaluates all the calendar cohorts for a population
evalCohorts
  :: Monad m => Population (Day, Integer) -> m (CohortMap (Integer, Bool) Day)
evalCohorts = makeCohortSpecsEvaluator defaultCohortEvalOptions specs

{-------------------------------------------------------------------------------
  Testing
  This would generally be in a separate file
-------------------------------------------------------------------------------}
-- m
--   :: Year -> MonthOfYear -> Int -> Integer -> [Text] -> ExampleModel -> Evnt
-- m y m d dur c dmn = event itv ctx where
--   ctx = context (packConcepts c) dmn Nothing
--   itv = beginerval dur (fromGregorian y m d)

-- testData1 :: [Evnt]
-- testData1 = sort
--   [ m 2010 1 1 1   ["is_female"]     (Demographics (Gender "Female"))
--   , m 2010 1 1 1   ["is_birth_year"] (Demographics (BirthYear 1960))
--   , m 2017 1 2 365 []    Enrollment
--   ]

-- testSubject1 :: Subject [Evnt]
-- testSubject1 = into ("a" :: Text, testData1)

-- testData2 :: [Evnt]
-- testData2 = sort
--   [ m 2010 1 1 1   ["is_female"]     (Demographics (Gender "Female"))
--   , m 2010 1 1 1   ["is_birth_year"] (Demographics (BirthYear 1980))
--   , m 2015 1 1 730 []    Enrollment
--   ]

-- testSubject2 :: Subject [Evnt]
-- testSubject2 = into ("b" :: Text, testData2)

-- testPop :: Population [Evnt]
-- testPop = into [testSubject1, testSubject2]

-- expectedObsUnita :: [ObsUnit () (Interval Day)]
-- expectedObsUnita = map from pairs where
--   pairs = zip
--     (fmap
--       (\(x, y) -> makeObsID (beginerval 1 (fromGregorian x y 1)) ("a" :: Text))
--       [(2017, 4), (2017, 7), (2017, 10)]
--     )
--     [(), (), ()]

-- makeExpectedCohort
--   :: AttritionInfo -> [ObsUnit ExampleModel ()] -> Cohort ExampleModel ()
-- makeExpectedCohort a x = MkCohort (a, into x)

-- expectedCohorts :: [Cohort () (Interval Day)]
-- expectedCohorts = zipWith
--   (curry MkCohort)
--   [ makeTestAttritionInfo
--     2
--     2
--     [ (SubjectHasNoIndex                     , 0)
--     , (ExcludedBy (1, "isEnrolled")          , 2)
--     , (Included                              , 0)
--     ]
--   , makeTestAttritionInfo
--     2
--     2
--     [ (SubjectHasNoIndex                     , 0)
--     , (ExcludedBy (1, "isEnrolled")          , 1)
--     , (Included                              , 1)
--     ]
--   , makeTestAttritionInfo
--     2
--     2
--     [ (SubjectHasNoIndex                     , 0)
--     , (ExcludedBy (1, "isEnrolled")          , 1)
--     , (Included                              , 1)
--     ]
--   , makeTestAttritionInfo
--     2
--     2
--     [ (SubjectHasNoIndex                     , 0)
--     , (ExcludedBy (1, "isEnrolled")          , 1)
--     , (Included                              , 1)
--     ]
--   ]
--   (fmap into ([] : transpose [expectedObsUnita]))

-- expectedCohortSet :: CohortMap () (Interval Day)
-- expectedCohortSet = into $ into @(Map Text (Cohort () (Interval Day))) $ zip
--   (fmap (pack . show) indices)
--   expectedCohorts

-- example :: TestTree
-- example = testGroup
--   "Unit tests for calendar cohorts"
--   [ testCase "expected Features for testData1"
--     $ -- Featureable cannot be tested for equality directly, hence encoding to
--       -- JSON bytestring and testing that for equality
--         encode (evalCohorts @[] testPop)
--     @?= encode [expectedCohortSet]
--   ]
