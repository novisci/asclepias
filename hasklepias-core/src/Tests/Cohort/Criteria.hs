-- | Tests for Cohort.Criteria. There is not much to test here except
-- 'firstExclude', but the tests help ensure any changes do no have unintended
-- consequences.

{-# LANGUAGE OverloadedStrings #-}
module Tests.Cohort.Criteria
  ( tests
  ) where

import           Cohort.Criteria
import qualified Data.List.NonEmpty    as NE
import           Data.Maybe            (isJust)
import           Data.Text             (Text, pack)
import           Test.Tasty
import           Test.Tasty.QuickCheck

  {- ORPHANS -}

-- TODO move this into Cohort.Criteria if used anywhere else.

instance Arbitrary Status where
  arbitrary = elements [Include, Exclude]

instance Arbitrary Criterion where
  arbitrary = MkCriterion . pack <$> arbitrary <*> arbitrary

  {- UTILITIES -}

isInclude :: Criterion -> Bool
isInclude = (== Include) . status

-- | Put @MkCriterion t Exclude@ before all others with status @Exclude@ in the
-- list.
makeThisExclusionFirst :: Text -> Criteria -> Criteria
makeThisExclusionFirst t = op . fmap (c :) . NE.span isInclude
  where c = MkCriterion t Exclude
        op (l1, l2) = NE.fromList (l1 <> l2)

-- | 'firstExclude' indeed always gets the first ExcludedBy element. Though the
-- function as currently written transparently assures that, this function
-- exists to catch unintended consequences of any future changes. Returning the
-- first exclusion reason is one of the guarantees Hasklepias currently
-- provides, as of 2023-01-03.
prop_firstExclude :: NonEmptyList Criterion -> Property
prop_firstExclude (NonEmpty xs) = isJust cfirst ==> cfirst === Just (MkCriterion "ALWAYS FIRST" Exclude)
  where cfirst = firstExclude $ makeThisExclusionFirst "ALWAYS FIRST" $ NE.fromList xs

  {- EXPECTED VALUES -}

  {- TESTS -}

tests :: TestTree
tests = testGroup
  "Tests on Cohort.Criteria"
  [testProperty "firstExclude gets first Exclude criterion" prop_firstExclude ]
