{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Cohort.Core
  ( tests
  ) where

import           Cohort
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Set                       ( empty
                                                , fromList
                                                , singleton
                                                )
import           Data.Text
import           Features
import           GHC.Natural
import           Test.Tasty
import           Test.Tasty.HUnit
import           Witch

{-
Test Cohort
-}

newtype SillySubjData = MkSillySubjData (Int, Bool, Bool, Text)
  deriving (Eq, Show)

c1 :: Definition (Feature "feat1" Bool -> Feature "crit1" Status)
c1 = define includeIf

c2 :: Definition (Feature "feat2" Bool -> Feature "crit2" Status)
c2 = define excludeIf

buildIndices :: SillySubjData -> IndexSet Int
buildIndices (MkSillySubjData (i, _, _, _)) | i <= 0    = makeIndexSet [i]
                                            | otherwise = makeIndexSet []

buildCriteria :: Int -> SillySubjData -> Criteria
buildCriteria _ (MkSillySubjData (_, b1, b2, _)) =
  criteria $ f c1 b1 : [f c2 b2]
  where f c d = criterion $ eval c (pure d)

buildFeatures :: Int -> SillySubjData -> Text
buildFeatures _ (MkSillySubjData (_, _, _, t)) = t

cohort2spec :: CohortSpec SillySubjData Text Int
cohort2spec = specifyCohort buildIndices buildCriteria buildFeatures

runCohort
  :: Monad m
  => CohortEvalOptions
  -> Population SillySubjData
  -> m (Cohort Text Int)
runCohort opts = makeCohortEvaluator opts cohort2spec

makePop :: [(Text, SillySubjData)] -> Population SillySubjData
makePop x = into (fmap (into @(Subject SillySubjData)) x)

makeTestObsID :: Text -> Int -> ObsID Int
makeTestObsID x y = into (x, y)

makeTestObsUnit :: Text -> Int -> Text -> ObsUnit Text Int
makeTestObsUnit x y z = into (makeTestObsID x y, z)

makeExpectedAttrition
  :: Int -> Int -> Natural -> Natural -> Natural -> Natural -> AttritionInfo
makeExpectedAttrition a b c d e f = makeTestAttritionInfo
  a
  b
  [ (SubjectHasNoIndex      , c)
  , (ExcludedBy (1, "crit1"), d)
  , (ExcludedBy (2, "crit2"), e)
  , (Included               , f)
  ]

makeExpected
  :: Int
  -> Int
  -> Natural
  -> Natural
  -> Natural
  -> Natural
  -> [(Text, Int, Text)]
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
  ]
