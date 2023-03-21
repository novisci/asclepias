-- | Tests of 'evalCohort' used to demonstrate the relevant guarantees listed
-- in the `Hasklepias` module documentation (see hasklepias-main).

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Tests.Cohort.Core (tests) where

import           Cohort.Cohort
import           Cohort.Core
import           Cohort.Criteria
import           Cohort.IndexSet    (IndexSet)
import qualified Cohort.IndexSet    as IS
import qualified Data.List          as L
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as M
import           Data.Semigroup
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Vector        as V
import           EventDataTheory
import           Test.Tasty
import           Test.Tasty.HUnit
import           Variable
import           Variable.Variable  (asVariableWrapped)


{- TYPES AND UTILITIES -}

-- Subject-level data

-- | 'Model', ie context for 'SillyEvent'.
data SillySubjData
  = MkSillySubjData
      { crit1 :: Bool
      , crit2 :: Bool
      , info  :: Text
      }
  deriving (Eq, Show)

type SillyEvent = Event () SillySubjData Int

type SillySubject = Subject () SillySubjData Int

-- Cohort-building logic and utilities

-- | Is 'crit1' satisfied? If so, 'Include'. Stand-in for a function that would
-- check model data and compute this.
c1 :: SillySubjData -> Criterion
c1 = MkCriterion "c1" . includeIf . crit1

-- | Is 'crit2' satisfied? If so, 'Exclude.
c2 :: SillySubjData -> Criterion
c2 = MkCriterion "c2" . excludeIf . crit2

-- | Single index of (begin i, begin i + 1) if the first event's @begin i@ is
-- non-positive, else no index.
buildIndices :: NonEmpty SillyEvent -> IndexSet Int
buildIndices (e :| _)
  | b <= 0    = IS.fromList [beginervalMoment b]
  | otherwise = IS.fromList []
  where b = begin $ getInterval e


-- | Cases with multiple indices, where you arbitrarily add a first index of
-- (101, 102) if i is non-positive, with no index otherwise.
multiIndices :: NonEmpty SillyEvent  -> IndexSet Int
multiIndices (e :| _)
  | b <= 0    = IS.fromList [beginervalMoment 101, beginervalMoment b]
  | otherwise = IS.fromList []
  where b = begin $ getInterval e

-- | Create @Criteria@ based on an index, @SillyEvent@ and @c1@, @c2@.
-- NOTE: The cohort status within the criteria do not depend on the index, only
-- on the context. So the added 101 index for the multi* tests does not affect
-- the status.
-- copied.
buildCriteria :: NonEmpty SillyEvent -> Interval Int -> Criteria
buildCriteria (e :| es) _ = sconcat $ NE.map op (e :| es)
  where op e' = let d' = getFacts $ getContext e'
                in c1 d' :| [c2 d']

-- NOTE: It's not necessary to use RTypeRep at this point. You could just
-- return Text. Done for demo purposes.
infoVariable :: SillyEvent -> RTypeRep 'STRSXP
infoVariable = as_character . info . getFacts . getContext

toMultiInfo :: Interval Int -> Text -> Text
toMultiInfo i t = T.pack (show i) <> ": " <> t

-- To be used with multi* test data. Example of the annoyance of using 'Maybe'
-- in RTypeRep.
multiinfoVariable :: Interval Int -> SillyEvent -> RTypeRep 'STRSXP
multiinfoVariable i = V.map (fmap (toMultiInfo i)) . infoVariable

-- | Stand-in for output variable construction based on an index and event.
-- Simply grabs the 'info' field from each event and concats. Variables are
-- named arbitrarily.
buildVariables :: NonEmpty SillyEvent -> Interval Int -> VariableRow
buildVariables es _ = zipWith (\nm e -> rVector nm $ infoVariable e) nms (NE.toList es)
  where nms = map (\i -> "v" <> T.pack (show i)) [1..length es]

-- | Like the above, but the feature now declares which index it is associated
-- with. NOTE 'NE.fromList' will throw an exception if the list is empty.
multibuildVariables :: NonEmpty SillyEvent -> Interval Int -> VariableRow
multibuildVariables es i = zipWith (\nm e -> rVector nm $ multiinfoVariable i e) nms (NE.toList es)
  where nms = map (\i' -> "v" <> T.pack (show i')) [1..length es]

-- | This is what the user creates: entry-point to the CohortApp pipeline.
cohort2spec :: CohortSpec () SillySubjData Int
cohort2spec = MkCohortSpec buildIndices buildCriteria buildVariables

multicohort2spec :: CohortSpec () SillySubjData Int
multicohort2spec = MkCohortSpec multiIndices buildCriteria multibuildVariables

-- | "run" the cohort based on the provided logic in the spec. note this only
-- evaluates pure code, so it is not the same as the runner the CohortApp
-- actually would use. this module does not test the effectful CohortApp.
runCohort :: [SillySubject] -> Cohort Int
runCohort = evalCohort cohort2spec

-- | Example where multiple indices allowed.
multirunCohort :: [SillySubject] -> Cohort Int
multirunCohort = evalCohort multicohort2spec

{- DATA and DATA-CREATION UTILITIES -}

-- Utilities

-- | Initial SillySubjData, which can be modified in creating data.
-- Subject with this data is included (crit1 == True, crit2 == False).
dummyData :: Context () SillySubjData
dummyData = context (packTagSet []) d Nothing
  where d = MkSillySubjData True False ""

-- | Silly setters.
setCrit1, setCrit2 :: Bool -> Context () SillySubjData -> Context () SillySubjData
setCrit1 x c = context ts d Nothing
  where ts = getTagSet c
        d0 = getFacts c
        d = d0 { crit1 = x }

setCrit2 x c = context ts d Nothing
  where ts = getTagSet c
        d0 = getFacts c
        d = d0 { crit2 = x }

setInfo :: Text -> Context () SillySubjData -> Context () SillySubjData
setInfo x c = context ts d Nothing
  where ts = getTagSet c
        d0 = getFacts c
        d = d0 { info = x }

-- Example data

-- TODO once subjects without index are properly handled as errors, these
-- tests should evaluate the error state.

-- TODO see note about buildIndices above. at the moment, only the first event
-- can be an index. that should change in a future round of edits.

-- | Subject without index, since the only event has interval (1, 2).
-- Specs: This subject should not appear in the Cohort data, only contributing
-- +1 to subjectsProcessed in the AttritionInfo.
noIx :: SillySubject
noIx = MkSubject (MkSubjId "noix") $ NE.fromList [event (beginervalMoment 1) dummyData]

-- | Subject who is excluded by c1, with a single index.
-- Specs: This subject should not appear in the Cohort data. It should contribute to the AttritionInfo by adding
-- +1 to subjectsProcessed, +1 to unitsProcessed and +1 to the @ExcludedBy
-- "c1"@ element of attritionByStatus. This subject should contribute no
-- 'ObsUnit' data to the cohort.
excludedByC1SingleIx :: SillySubject
excludedByC1SingleIx = MkSubject (MkSubjId "excludedByC1SingleIx") (NE.fromList es)
  where es = [event (beginervalMoment (-1)) (setInfo "i'm in, c1" dummyData)
             -- NOTE: this event does not contribute to the IndexSet, but it is
             -- still part of the subject's data and therefore is used to
             -- compute criteria.
             , event (beginervalMoment 2) (setInfo "i'm not, c1" $ setCrit1 False dummyData)
             ]

-- | Subject excluded by c2, with single index.
-- Specs: This subject should not appear in the Cohort data. It should contribute to the AttritionInfo by adding
-- +1 to subjectsProcessed, +1 to unitsProcessed and +1 to the @ExcludedBy
-- "c2"@ element of attritionByStatus.
excludedByC2SingleIx :: SillySubject
excludedByC2SingleIx = MkSubject (MkSubjId "excludedByC2SingleIx") (NE.fromList es)
  where es = [event (beginervalMoment (-2)) (setInfo "i'm not, c2" $ setCrit2 True dummyData)
             ]


-- | Subject excluded by c1, with multiple Exclude values, with single index.
-- Specs: This subject should not contribute to the 'cohortData'. It should be
-- excluded by the first Exclude, which has label "i'm not, c1". It should
-- contribute to the AttritionInfo by adding +1 to subjectsProcessed, +1 to
-- unitsProcessed and +1 to the @ExcludedBy "c1"@ element of attritionByStatus.
excludedByC1SingleIx' :: SillySubject
excludedByC1SingleIx' = MkSubject (MkSubjId "excludedByC1SingleIx'") (NE.fromList es)
  where es = [event (beginervalMoment (-1)) (setInfo "i'm not, c1" $ setCrit1 False dummyData)
             -- NOTE this does not contribute to the IndexSet, since only the
             -- first date is used in buildIndices.
             , event (beginervalMoment (-2)) (setInfo "i'm not, c2" $ setCrit2 True dummyData)
             ]

-- | Subject included.
-- Specs: This subject should contribute to the 'cohortData'. It should
-- contribute to the AttritionInfo byadding +1 to subjectsProcessed,
-- unitsProcessed and to the @Included@ count of attritionByStatus.
includedSingleIx :: SillySubject
includedSingleIx = MkSubject (MkSubjId "includedSingleIx") (NE.fromList es)
  where es = [event (beginervalMoment (-1)) (setInfo "i'm in, c1" dummyData)
             -- NOTE this does not contribute to the IndexSet, since only the
             -- first date is used in buildIndices.
             , event (beginervalMoment (-2)) (setInfo "i'm in, c2" dummyData)
             ]

-- | Subject included.
-- Specs: This subject should contribute to the 'cohortData'. It should
-- contribute to the AttritionInfo byadding +1 to subjectsProcessed,
-- unitsProcessed and to the @Included@ count of attritionByStatus.
includedSingleIx' :: SillySubject
includedSingleIx' = MkSubject (MkSubjId "includedSingleIx'") (NE.fromList es)
  where es = [event (beginervalMoment (-1)) (setInfo "i'm in too, c1" dummyData)
             , event (beginervalMoment (-2)) (setInfo "i'm in too, c2" dummyData)
             ]


-- Cohorts

-- Legend (regex):
-- ^cohortSingleIx([EI][12]?){1,4}'?$
-- E: Excluded
-- I: Included
-- i: ExcludedBy ci
-- ': denotes use of one or more "'" variants
--
-- e.g. "cohortE1E2" is the cohort created from [excludedByC1SingleIx, excludedByC2SingleIx]

-- This stands alone and doesn't conform to pattern.
cohortNoIx :: Cohort Int
cohortNoIx = runCohort [noIx]

cohortSingleIxE1 :: Cohort Int
cohortSingleIxE1 = runCohort [excludedByC1SingleIx]

cohortSingleIxE1' :: Cohort Int
cohortSingleIxE1' = runCohort [excludedByC1SingleIx']

cohortSingleIxE2 :: Cohort Int
cohortSingleIxE2 = runCohort [excludedByC2SingleIx]

cohortSingleIxI :: Cohort Int
cohortSingleIxI = runCohort [includedSingleIx]

cohortSingleIxIE1 :: Cohort Int
cohortSingleIxIE1 = runCohort [includedSingleIx, excludedByC1SingleIx]

cohortSingleIxIE1E2 :: Cohort Int
cohortSingleIxIE1E2 = runCohort [includedSingleIx, excludedByC1SingleIx, excludedByC2SingleIx]

cohortSingleIxIE1E2' :: Cohort Int
cohortSingleIxIE1E2' = runCohort [includedSingleIx
                                 , excludedByC1SingleIx
                                 , excludedByC1SingleIx'
                                 , excludedByC2SingleIx
                                 ]


cohortSingleIxIIE1E2' :: Cohort Int
cohortSingleIxIIE1E2' = runCohort [includedSingleIx
                                  , includedSingleIx'
                                  , excludedByC1SingleIx
                                  , excludedByC2SingleIx
                                  ]

-- Multiple (artificial) indices
cohortMultiIxIE1E2 :: Cohort Int
cohortMultiIxIE1E2 = multirunCohort [includedSingleIx, excludedByC1SingleIx, excludedByC2SingleIx]

  {- EXPECTED Variable Data -}

-- Where possible, expected values are spciefied in the test itself, but
-- constructing features is verbose.

-- | Utility for comparing lists of [ObsUnit Int]. 'allEqVariableableData' sorts
-- before comparing via ShapeOutput and ToJSON. The length check is important
-- since zip shortens the result to the min length between the two lists.
compareCohortData :: [ObsUnit Int] -> [ObsUnit Int] -> Bool
compareCohortData xs ys = (length xs == length ys) && and bs
  where bs = zipWith (\o1 o2 -> allEqVariableRow (obsData o1) (obsData o2)) xs' ys'
        xs' = L.sortBy (\x y -> compare (obsId x) (obsId y)) xs
        ys' = L.sortBy (\x y -> compare (obsId x) (obsId y)) ys
        -- NOTE: A lazy comparison of Variables by their VariableWrapped (i.e.
        -- JSON) representations.
        allEqVariableRow vs1 vs2 = L.sort (map asVariableWrapped vs1) == L.sort (map asVariableWrapped vs2)

-- | Utility for making a VariableRow from a list of Text. Explicitly do not
-- want to reuse the `buildVariables` functions, since it doing so would be
-- close to producing a tautological test. Variable names assigned arbitrarily.
textVariableRow :: [Text] -> VariableRow
textVariableRow xs = zipWith (\nm t -> rVector nm $ as_character t) nms xs
  where nms = map (\i -> "v" <> T.pack (show i)) [1..length xs]

expectedNoIx :: [ObsUnit Int]
expectedNoIx = []

-- | Expected obsData from 'cohortSingleIxE1'. There is one excluded subject,
-- so no data is expected in the current implementation.
expectedSingleIxE1 :: [ObsUnit Int]
expectedSingleIxE1 = []

-- | Expected obsData from 'cohortSingleIxI'
expectedSingleIxI :: [ObsUnit Int]
expectedSingleIxI = [o1]
  where oid1 = MkObsId (MkSubjId "includedSingleIx") (beginervalMoment (-1))
        o1 = MkObsUnit oid1 $ textVariableRow ["i'm in, c1", "i'm in, c2"]

-- | Expected obsData from 'cohortSingleIxIE1E2'.
expectedSingleIxIE1E2 :: [ObsUnit Int]
expectedSingleIxIE1E2 = expectedSingleIxI

-- | Expected obsData from 'cohortSingleIxIIE1E2\''.
expectedSingleIxIIE1E2' :: [ObsUnit Int]
expectedSingleIxIIE1E2' = o2 : expectedSingleIxI
  where oid2 = MkObsId (MkSubjId "includedSingleIx'") (beginervalMoment (-1))
        o2 = MkObsUnit oid2 $ textVariableRow ["i'm in too, c1", "i'm in too, c2"]

-- | Expected obsData from 'cohortMultiIxIE1E2'.
expectedMultiIxIE1E2 :: [ObsUnit Int]
expectedMultiIxIE1E2 = [o1, o2]
  where oid1 = MkObsId (MkSubjId "includedSingleIx") (beginervalMoment (-1))
        o1 = MkObsUnit oid1 d1
        oid2 = MkObsId (MkSubjId "includedSingleIx") (beginervalMoment 101)
        o2 = MkObsUnit oid2 d2
        -- These differ only in the index time prepended.
        d1 = textVariableRow ["(-1, 0): i'm in, c1", "(-1, 0): i'm in, c2"]
        d2 = textVariableRow ["(101, 102): i'm in, c1", "(101, 102): i'm in, c2"]

{- TESTS -}

-- | Single-index tests, AttritionInfo
testsSingleIxAttrition :: TestTree
testsSingleIxAttrition = testGroup
  "Single-index tests, AttritionInfo"
  [testCase "AttritionInfo for noIx" $
    attritionInfo cohortNoIx @?=
      MkAttritionInfo 1 0 M.empty
  , testCase "AttritionInfo for single exclusion, c1" $
    attritionInfo cohortSingleIxE1 @?=
      MkAttritionInfo 1 1 (M.fromList [(ExcludedBy "c1", 1)])
  , testCase "AttritionInfo for single exclusion, c2" $
    attritionInfo cohortSingleIxE2 @?=
      MkAttritionInfo 1 1 (M.fromList [(ExcludedBy "c2", 1)])
  , testCase "AttritionInfo for multi exclusion, excludeby c1" $
    attritionInfo cohortSingleIxE1' @?=
      MkAttritionInfo 1 1 (M.fromList [(ExcludedBy "c1", 1)])
  , testCase "AttritionInfo for single inclusion" $
    attritionInfo cohortSingleIxI @?=
      MkAttritionInfo 1 1 (M.fromList [(Included, 1)])
  , testCase "AttritionInfo for one inclusion, one exclusion by c1" $
    attritionInfo cohortSingleIxIE1 @?=
      MkAttritionInfo 2 2 (M.fromList [(Included, 1), (ExcludedBy "c1", 1)])
  , testCase "AttritionInfo for one inclusion, one exclusion by c1, one by c2" $
    attritionInfo cohortSingleIxIE1E2 @?=
      MkAttritionInfo 3 3 (M.fromList [(Included, 1), (ExcludedBy "c1", 1), (ExcludedBy "c2", 1)])
  , testCase "AttritionInfo for one inclusion, two exclusion by c1, one by c2" $
    attritionInfo cohortSingleIxIE1E2' @?=
      MkAttritionInfo 4 4 (M.fromList [(Included, 1), (ExcludedBy "c1", 2), (ExcludedBy "c2", 1)])
  ]

-- | Single-index tests, cohortData
testsSingleIxData :: TestTree
testsSingleIxData = testGroup
  "Single-index tests, cohortData"
  -- Equality expected.
  [testCase "cohortData for cohort where no subjects have index times" $
    compareCohortData (cohortData cohortNoIx) expectedNoIx @? "Expected True"
   , testCase "cohortData for single exclusion" $
    compareCohortData (cohortData cohortSingleIxE1) expectedSingleIxE1 @? "Expected True"
   , testCase "cohortData for single inclusion" $
    compareCohortData (cohortData cohortSingleIxI) expectedSingleIxI @? "Expected True"
   , testCase "cohortData for single inclusion, two exclusions" $
    compareCohortData (cohortData cohortSingleIxIE1E2) expectedSingleIxIE1E2 @? "Expected True"
   , testCase "cohortData for two inclusions, two exclusions" $
    compareCohortData (cohortData cohortSingleIxIIE1E2') expectedSingleIxIIE1E2' @? "Expected True"
  -- Not equals expected.
   , testCase "cohortData for single exclusion" $
    not (compareCohortData (cohortData cohortSingleIxE1) expectedSingleIxI) @? "Expected False"
   , testCase "cohortData for single inclusion" $
    not (compareCohortData (cohortData cohortSingleIxI) expectedSingleIxIIE1E2') @? "Expected False"
  ]


-- | Multi-index tests, cohortData
testsMultiIxData :: TestTree
testsMultiIxData = testGroup
  "Multi-index tests, cohortData"
  [testCase "cohortData for cohort with single included subject contributing two units" $
    compareCohortData (cohortData cohortMultiIxIE1E2) expectedMultiIxIE1E2 @? "Expected True"
  ]

-- | All tests
tests :: TestTree
tests = testGroup
  "Unit tests on Cohort.Core"
  [testsSingleIxAttrition, testsSingleIxData, testsMultiIxData]
