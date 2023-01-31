-- | Tests the utilities of Cohort.Cohort. Those functions are essential for
-- computing attrition counts correctly and for performing grouping operations
-- for lists of events. See Tests.Cohort.Core for tests of the
-- cohort-processing pipeline's pure functions.
{-# LANGUAGE OverloadedStrings #-}
module Tests.Cohort.Cohort where

import           Cohort.Cohort
import           Data.Foldable         (foldl')
import           Data.List             (nubBy, sort)
import           Data.Map.Strict       (fromList)
import           Data.Text             (Text, pack)
import           EventDataTheory       (Event)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck


  {- UTILITIES -}

-- Orphan instance used only for testing here. If needed elsewhere, define it
-- using a newtype wrapper and place in Cohort somewhere.
instance Arbitrary Text where
  arbitrary = pack <$> (3 `resize` arbitrary)

  {- DATA -}

-- | One subject, two units.
testAttr1 :: AttritionInfo
testAttr1 = MkAttritionInfo 1 2 m
  where m = fromList [(ExcludedBy "feat2", 1), (Included, 1)]

-- | One subject, five units.
testAttr2 :: AttritionInfo
testAttr2 = MkAttritionInfo 1 5 m
  where m = fromList [(ExcludedBy "feat1", 1), (ExcludedBy "feat2", 1), (Included, 2)]

-- | One subject, one unit.
testAttr3 :: AttritionInfo
testAttr3 = MkAttritionInfo 1 1 m
  where m = fromList [(ExcludedBy "feat3", 1)]

-- | Two subjects, seven units.
testAttr4 :: AttritionInfo
testAttr4 = MkAttritionInfo 2 7 m
  where m = fromList [(ExcludedBy "feat1", 2), (ExcludedBy "feat2", 2), (Included, 3)]

-- Event grouping

-- | A crude check that grouping was done correctly.
prop_eventgrouping :: NonEmptyList (Text, Event () () Int) -> Property
prop_eventgrouping (NonEmpty xs) = sid === sid0 .&&. nes === length xs
  where ss = eventsToSubject xs
        -- Need to sort this, since eventsToSubject sorts
        sid0 = sort $ map fst $ nubBy (\(t1, _) (t2, _) -> t1 == t2) xs
        sid = map ((\(MkSubjId i) -> i) . subjId) $ nubBy (\s1 s2 -> subjId s1 == subjId s2) ss
        nes = foldl' op 0 ss where op n s = n + length (subjData s)

  {- EXPECTED -}

-- AttritionInfo

-- | Zero subjectsProcessed, one unit, one Included. NOTE: this is an
-- AttritionInfo "in process", intended to have zero subjectsProcessed count.
incrementEmptyByIncluded :: AttritionInfo
incrementEmptyByIncluded = MkAttritionInfo 0 1 $ fromList [(Included, 1)]

-- | Update testAttr3 for an additional Excluded unit with differing label
incrementTestAttr3ByExcludedNew :: AttritionInfo
incrementTestAttr3ByExcludedNew = MkAttritionInfo 1 2 m
  where m = fromList [(ExcludedBy "feat3", 1), (ExcludedBy "feat2", 1)]

-- | Update testAttr3 for an additional Excluded unit with same label
incrementTestAttr3ByExcludedSame :: AttritionInfo
incrementTestAttr3ByExcludedSame = MkAttritionInfo 1 2 m
  where m = fromList [(ExcludedBy "feat3", 2)]

-- | Combine testAttr1 and testAttr2
combinedAttr12 :: AttritionInfo
combinedAttr12 = MkAttritionInfo 2 7 m
  where m = fromList [(ExcludedBy "feat1", 1), (ExcludedBy "feat2", 2), (Included, 3)]

-- | Combine testAttr3 and testAttr4
combinedAttr34 :: AttritionInfo
combinedAttr34 = MkAttritionInfo 3 8 m
  where m = fromList [(ExcludedBy "feat3", 1), (ExcludedBy "feat1", 2), (ExcludedBy "feat2", 2), (Included, 3)]


{- TESTS -}

-- AttritionInfo
testAttrition :: TestTree
testAttrition = testGroup
  "Tests of AttritionInfo utilities"
  [testCase "increment empty with Included" $
    incrementAttritionForUnit Included emptyAttrition @?= incrementEmptyByIncluded
  , testCase "increment testAttr3 with differing ExcludedBy" $
    incrementAttritionForUnit (ExcludedBy "feat2") testAttr3 @?= incrementTestAttr3ByExcludedNew
  , testCase "increment testAttr3 with same ExcludedBy" $
    incrementAttritionForUnit (ExcludedBy "feat3") testAttr3 @?= incrementTestAttr3ByExcludedSame
  , testCase "combine testAttr1 and testAttr2" $
    combineAttrition testAttr1 testAttr2 @?= combinedAttr12
  , testCase "combine testAttr3 and testAttr4" $
    combineAttrition testAttr3 testAttr4 @?= combinedAttr34
  ]

-- Event grouping

testEventsToSubject :: TestTree
testEventsToSubject = testGroup
  "Test of eventsToSubject"
  [testProperty "Groups correctly" prop_eventgrouping]

-- All
tests :: TestTree
tests = testGroup
  "Unit tests on Cohort.Cohort"
  [testAttrition, testEventsToSubject]
