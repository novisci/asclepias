{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Tests.Cohort.Core
  ( tests
  ) where

import           Cohort
import           Data.Set         (empty, fromList, singleton)
import           Data.Text
import           Features
import           GHC.Natural
import           Test.Tasty
import           Test.Tasty.HUnit
import           Witch


{-
This example is purely here to check that GHC can relatively easily infer
types based on the From instances for subject/population types
-}
examplePopulation :: Population (Bool, Integer)
examplePopulation =
  from @[(Int, (Bool, Integer))] [(1, (True, 5)), (2, (True, 0))]


{-
Test Cohort
-}

newtype SillySubjData
  = MkSillySubjData (Int, Bool, Bool, Text)
  deriving (Eq, Show)

c1 :: Definition (Feature "feat1" Bool -> Feature "crit1" Status)
c1 = define includeIf

c2 :: Definition (Feature "feat2" Bool -> Feature "crit2" Status)
c2 = define excludeIf



-- Single index if i is non-positive, else no index.
buildIndices :: SillySubjData -> IndexSet Int
buildIndices (MkSillySubjData (i, _, _, _)) | i <= 0    = makeIndexSet [i]
                                            | otherwise = makeIndexSet []


-- Cases with multiple indices, where you arbitrarily add a first index of 101
-- if i is non-positive, with no index otherwise.
multiIndices :: SillySubjData -> IndexSet Int
multiIndices (MkSillySubjData (i, _, _, _)) 
  | i <= 0    = makeIndexSet [101, i] 
  | otherwise = makeIndexSet []

-- NOTE: The cohort status within the criteria do not depend on the index, only
-- on the booleans. So the added 101 index for the multi* tests does not affect
-- the status.
buildCriteria :: Int -> SillySubjData -> Criteria
buildCriteria _ (MkSillySubjData (_, b1, b2, _)) =
  into @Criteria $ f c1 b1 : [f c2 b2]
  where f c d = into @CriterionThatCanFail $ eval c (pure d)

-- Simply grabs the subject-level data, the Text element in the quadruple of
-- SillySubjData.
buildFeatures :: Int -> SillySubjData -> Text
buildFeatures _ (MkSillySubjData (_, _, _, t)) = t

-- Like buildFeatures, but the feature now declares which index it is
-- associated with.
multibuildFeatures :: Int -> SillySubjData -> Text
multibuildFeatures i (MkSillySubjData (_, _, _, t)) = pack (show i ++ ": ") <> t

cohort2spec :: CohortSpec SillySubjData Text Int
cohort2spec = specifyCohort buildIndices buildCriteria buildFeatures

multicohort2spec :: CohortSpec SillySubjData Text Int
multicohort2spec = specifyCohort multiIndices buildCriteria multibuildFeatures

runCohort
  :: Monad m
  => CohortEvalOptions
  -> Population SillySubjData
  -> m (Cohort Text Int)
runCohort opts = makeCohortEvaluator opts cohort2spec

-- runCohort, but with multiple indices allowed.
multirunCohort
  :: Monad m
  => CohortEvalOptions
  -> Population SillySubjData
  -> m (Cohort Text Int)
multirunCohort opts = makeCohortEvaluator opts multicohort2spec

makePop :: [(Text, SillySubjData)] -> Population SillySubjData
makePop x = into (fmap (into @(Subject SillySubjData)) x)

makeTestObsID 
  :: Text -- ^ Subject id
  -> Int -- ^ temporal index (here i type in cohort is not an interval but plain Int)
  -> ObsID Int
makeTestObsID x y = into (x, y)

makeTestObsUnit 
  :: Text -- ^ subject id
  -> Int -- ^ temporal index (here i type in cohort is not an interval but plain Int)
  -> Text -- ^ cohort "data", the output of the feature runner here
  -> ObsUnit Text Int
makeTestObsUnit x y z = into (makeTestObsID x y, z)

makeExpectedAttrition
  :: Int     -- ^ count of subjects
  -> Int     -- ^ count of observational units
  -> Natural -- ^ count for SubjectHasNoIndex
  -> Natural -- ^ count for ExcludedBy (1, "crit1")
  -> Natural -- ^ count for ExcludedBy (2, "crit2")
  -> Natural -- ^ count for Included
  -> AttritionInfo
makeExpectedAttrition a b c d e f = makeTestAttritionInfo
  a
  b
  [ (SubjectHasNoIndex      , c)
  , (ExcludedBy (1, "crit1"), d)
  , (ExcludedBy (2, "crit2"), e)
  , (Included               , f)
  ]

-- Expected Cohort d0 i, with d0 Text and i not an interval but Int
makeExpected
  :: Int     -- ^ count of subjects
  -> Int     -- ^ count of observational units
  -> Natural -- ^ count for SubjectHasNoIndex
  -> Natural -- ^ count for ExcludedBy (1, "crit1")
  -> Natural -- ^ count for ExcludedBy (2, "crit2")
  -> Natural -- ^ count for Included
  -> [(Text, Int, Text)] -- ^ List of (Subject id, temporal index, output of feature runner)
  -> Cohort Text Int
makeExpected a b c d e f g = MkCohort
  ( makeExpectedAttrition a b c d e f
  , into @(CohortData Text Int) (fmap (\(x, y, z) -> makeTestObsUnit x y z) g)
  )

makeCase
  :: CohortEvalOptions
  -> [(Text, SillySubjData)]
  -> Cohort Text Int
  -> Assertion
makeCase opts inputs expected =
  runCohort opts (makePop inputs) @?= pure @[] expected

multimakeCase
  :: CohortEvalOptions
  -> [(Text, SillySubjData)]
  -> Cohort Text Int
  -> Assertion
multimakeCase opts inputs expected =
  multirunCohort opts (makePop inputs) @?= pure @[] expected

-- Criteria with failure
-- The sole difference between these cases and the single-index cases above are
-- that the criteria have an error.
c3 :: Feature "crit3" Status
c3 = makeFeature $ featureDataL (CustomFlag "bad")

failbuildCriteria :: Int -> SillySubjData -> Criteria
failbuildCriteria _ (MkSillySubjData (_, b1, b2, _)) =
  into @Criteria $ f c1 b1 : [into @CriterionThatCanFail c3, f c2 b2]
  where f c d = into @CriterionThatCanFail $ eval c (pure d)

failcohort2spec :: CohortSpec SillySubjData Text Int
failcohort2spec = specifyCohort buildIndices failbuildCriteria buildFeatures

failrunCohort
  :: Monad m
  => CohortEvalOptions
  -> Population SillySubjData
  -> m (Cohort Text Int)
failrunCohort opts = makeCohortEvaluator opts failcohort2spec

failmakeCase
  :: CohortEvalOptions
  -> [(Text, SillySubjData)]
  -> Cohort Text Int
  -> Assertion
failmakeCase opts inputs expected =
  failrunCohort opts (makePop inputs) @?= pure @[] expected

{-
All tests
-}

tests :: TestTree
tests = testGroup
  "Unit tests on Cohort.Core"
  [ testCase "no subjects" $ makeCase
    defaultCohortEvalOptions
    []
    (MkCohort (mempty, into @(CohortData Text Int) ([] :: [ObsUnit Text Int])))
  , testCase "one included subject (default options)" $ makeCase
    defaultCohortEvalOptions
    [("a", MkSillySubjData (0, True, False, "keep me"))]
    (makeExpected 1 1 0 0 0 1 [("a", 0, "keep me")])
  , testCase "one subject without index (default options)" $ makeCase
    defaultCohortEvalOptions
    [("a", MkSillySubjData (1, True, False, "no index"))]
    (MkCohort
      ( makeTestAttritionInfo 1 0 [(SubjectHasNoIndex, 1), (Included, 0)]
      , into @(CohortData Text Int) ([] :: [ObsUnit Text Int])
      )
    )
  , testCase "one subject excluded by crit1 (default options)" $ makeCase
    defaultCohortEvalOptions
    [("a", MkSillySubjData (0, False, False, "excluded by crit1"))]
    (makeExpected 1 1 0 1 0 0 [])
  , testCase "one subject excluded by crit2 (default options)" $ makeCase
    defaultCohortEvalOptions
    [("a", MkSillySubjData (0, True, True, "excluded by crit2"))]
    (makeExpected 1 1 0 0 1 0 [])
  , testCase "one included subject (skipFeatures)" $ makeCase
    (MkCohortEvalOptions SkipFeatures AllSubjects)
    [("a", MkSillySubjData (0, True, False, "keep me"))]
    (makeExpected 1 1 0 0 0 1 [])
  , testCase "one included subject (exclude via sample)" $ makeCase
    (MkCohortEvalOptions OnlyOnIncluded (SubjectExludeList ["a"]))
    [("a", MkSillySubjData (0, True, False, "keep me"))]
    (MkCohort (mempty, into @(CohortData Text Int) ([] :: [ObsUnit Text Int])))
  , testCase "one excluded subject (run features on all)" $ makeCase
    (MkCohortEvalOptions OnAll AllSubjects)
    [("a", MkSillySubjData (0, False, False, "excluded by crit1"))]
    (makeExpected 1 1 0 1 0 0 [("a", 0, "excluded by crit1")])

  , testCase "3 subjects: all included (default options)" $ makeCase
    defaultCohortEvalOptions
    [ ("a", MkSillySubjData (0, True, False, "keep me"))
    , ("b", MkSillySubjData (-1, True, False, "keep me"))
    , ("c", MkSillySubjData (-2, True, False, "keep me"))
    ]
    (makeExpected
      3
      3
      0
      0
      0
      3
      [("a", 0, "keep me"), ("b", -1, "keep me"), ("c", -2, "keep me")]
    )

  -- Multi-index case
  -- This test was added to evaluate the bug in
  -- https://gitlab.com/TargetRWE/epistats/nsstat/asclepias/-/issues/350
  , testCase "Multi-index: 1 subjects: all included (default options)" $ multimakeCase
    -- Default options are OnlyOnIncluded, meaning features are run only on obs
    -- units with status Included
    defaultCohortEvalOptions
    -- (subject id, MkSillySubjData (index, crit1, crit2, subject data on which
    -- features are run)
    [ ("a", MkSillySubjData (0, True, False, "keep me")) ]
    -- See `makeExpected` docstrings for the meaning of the first arguments
    (makeExpected
      1
      2
      0
      0
      0
      2
      -- Should have one ObsUnit for each index.
      [("a", 0, "0: keep me"), ("a", 101, "101: keep me")]
    ), testCase "Multi-index: 2 subjects: all included (default options)" $ multimakeCase
    -- Default options are OnlyOnIncluded, meaning features are run only on obs
    -- units with status Included
    defaultCohortEvalOptions
    -- (subject id, MkSillySubjData (index, crit1, crit2, subject data on which
    -- features are run)
    [ ("a", MkSillySubjData (0, True, False, "keep me"))
    , ("b", MkSillySubjData (0, True, False, "keep me"))
    ]
    -- See `makeExpected` docstrings for the meaning of the first arguments
    (makeExpected
      2
      4
      0
      0
      0
      4
      -- Should have one ObsUnit for each index.
      [("a", 0, "0: keep me"), ("a", 101, "101: keep me"), ("b", 0, "0: keep me"), ("b", 101, "101: keep me")]
    )
  , testCase "3 subjects: 1 with no index, 2 included (default options)"
    $ makeCase
        defaultCohortEvalOptions
        [ ("a", MkSillySubjData (1, True, False, "keep me"))
        , ("b", MkSillySubjData (-1, True, False, "keep me"))
        , ("c", MkSillySubjData (-2, True, False, "keep me"))
        ]
        (makeExpected 3 2 1 0 0 2 [("b", -1, "keep me"), ("c", -2, "keep me")])
  , testCase
      "3 subjects: 1 exclude by crit1, 1 excluded by crit2, 1 included (default options)"
    $ makeCase
        defaultCohortEvalOptions
        [ ("a", MkSillySubjData (0, False, False, "excluded by crit1"))
        , ("b", MkSillySubjData (-1, True, False, "keep me"))
        , ("c", MkSillySubjData (-2, True, True, "excluded by crit2"))
        ]
        (makeExpected 3 3 0 1 1 1 [("b", -1, "keep me")])
  , testCase
      "3 subjects: 1 exclude by crit, 1 excluded by crit2, 1 included (run features on all)"
    $ makeCase
        (MkCohortEvalOptions OnAll AllSubjects)
        [ ("a", MkSillySubjData (0, False, False, "excluded by crit1"))
        , ("b", MkSillySubjData (-1, True, False, "keep me"))
        , ("c", MkSillySubjData (-2, True, True, "excluded by crit2"))
        ]
        (makeExpected
          3
          3
          0
          1
          1
          1
          [ ("a", 0 , "excluded by crit1")
          , ("b", -1, "keep me")
          , ("c", -2, "excluded by crit2")
          ]
        )
  , testCase
      "3 subjects: 1 exclude by crit, 1 excluded by crit2, 1 included (take only first2)"
    $ makeCase
        (MkCohortEvalOptions OnlyOnIncluded (FirstNSubjects 2))
        [ ("a", MkSillySubjData (0, False, False, "excluded by crit1"))
        , ("b", MkSillySubjData (-1, True, False, "keep me"))
        , ("c", MkSillySubjData (-2, True, True, "excluded by crit2"))
        ]
        (makeExpected 2 2 0 1 0 1 [("b", -1, "keep me")])
  , testCase
      "3 subjects: 1 exclude by crit, 1 excluded by crit2, 1 included (exclude the included)"
    $ makeCase
        (MkCohortEvalOptions OnlyOnIncluded (SubjectExludeList ["b"]))
        [ ("a", MkSillySubjData (0, False, False, "excluded by crit1"))
        , ("b", MkSillySubjData (-1, True, False, "keep me"))
        , ("c", MkSillySubjData (-2, True, True, "excluded by crit2"))
        ]
        (makeExpected 2 2 0 1 1 0 [])
  , testCase
      "case with failures"
    $ failmakeCase
        defaultCohortEvalOptions
        [ ("a", MkSillySubjData (0, False, False, "excluded by crit1"))
        , ("b", MkSillySubjData (-1, True, False, "keep me"))
        , ("c", MkSillySubjData (-2, True, True, "excluded by crit2"))
        ]
        (MkCohort (makeTestAttritionInfo 3 3
          [ (SubjectHasNoIndex    , 0)
          , (CriteriaFailure "CustomFlag \"bad\"", 3)
          , (Included             , 0)
          ] ,
          from @[ObsUnit Text Int] []))
  ]
