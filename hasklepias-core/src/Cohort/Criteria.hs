{-|
Module      : Cohort Criteria
Description : Defines the Criteria and related types and functions
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

module Cohort.Criteria
  ( Criterion
  , CriterionThatCanFail
  , CriterionFailure(..)
  , Criteria
  , Status(..)
  , CohortStatus(..)
  , makeCriterion
  , excludeIf
  , includeIf
  , checkCohortStatus
  , AttritionInfo
  , totalSubjectsProcessed
  , totalUnitsProcessed
  , attritionInfo
  , measureSubjectAttrition
  , makeTestAttritionInfo
  ) where

import           Data.Aeson      (FromJSON (..), FromJSONKey (..), ToJSON (..),
                                  ToJSONKey (..), object, (.=))
import           Data.Bifunctor  (Bifunctor (second))
import           Data.Binary
import           Data.List       (find)
import           Data.Map.Strict as Map (Map, fromListWith, unionsWith)
import           Data.Proxy      (Proxy (..))
import           Data.Text       (Text, pack)
import           Features.Core   (Feature, getData)
import           GHC.Exts        (IsList (..))
import           GHC.Generics    (Generic)
import           GHC.Num         (Natural)
import           GHC.TypeLits    (KnownSymbol, symbolVal)
import           Witch           (From (..))

{- |
Defines an observational unit's diposition in a cohort.
See @'checkCohortStatus'@ for evaluating a @'Criteria'@
to determine @CohortStatus@.
-}
data CohortStatus =
  -- | Indicates that a subject had no indices from which to derive
  --   observational units.
    SubjectHasNoIndex
  -- | Indicates that a @'CriterionThatCanFail'@ reached a @Left@ (failed) state.
  --  This status is only reachable when a user provides @'CriterionThatCanFail'@.
  | CriteriaFailure Text
  -- | Indicates that a unit is excluded in a cohort.
  --  The reason for exclusion is given by the @Text@ field.
  | ExcludedBy (Natural, Text)
  -- | Indicates that a unit is included in a cohort.
  | Included
    deriving (Eq, Show, Generic)

instance ToJSON CohortStatus
instance FromJSON CohortStatus
instance ToJSONKey CohortStatus
instance FromJSONKey CohortStatus
instance Binary CohortStatus

-- Defines an ordering to put @SubjectHasNoIndex@ first and @Included@ last.
-- The @'ExcludedBy'@ are ordered by their number value.
instance Ord CohortStatus where
  compare (CriteriaFailure x) (CriteriaFailure y) = compare x y
  compare (CriteriaFailure _) Included            = LT
  compare (CriteriaFailure _) SubjectHasNoIndex   = GT
  compare (CriteriaFailure _) (ExcludedBy      _) = LT
  compare Included            (CriteriaFailure _) = GT
  compare Included            Included            = EQ
  compare Included            SubjectHasNoIndex   = GT
  compare Included            (ExcludedBy      _) = GT
  compare (ExcludedBy      _) (CriteriaFailure _) = GT
  compare (ExcludedBy _)      Included            = LT
  compare (ExcludedBy _)      SubjectHasNoIndex   = GT
  compare (ExcludedBy (i, _)) (ExcludedBy (j, _)) = compare i j
  compare SubjectHasNoIndex   (CriteriaFailure _) = LT
  compare SubjectHasNoIndex   Included            = LT
  compare SubjectHasNoIndex   SubjectHasNoIndex   = EQ
  compare SubjectHasNoIndex   (ExcludedBy _     ) = LT

{-|
The @Text@ is a label for the @Status@.
The @'Status'@ identifies whether to @'Include'@ or @'Exclude'@ a subject.
-}
newtype Criterion = MkCriterion ( Text, Status ) deriving (Eq, Show)

-- | Defines the return type for @'Criterion'@ indicating whether to include or
-- exclude a subject.
data Status = Include | Exclude deriving (Eq, Show, Generic)

-- | Helper to convert a @Bool@ to a @'Status'@
--
-- >>> includeIf True
-- >>> includeIf False
-- Include
-- Exclude
includeIf :: Bool -> Status
includeIf True  = Include
includeIf False = Exclude

-- | Helper to convert a @Bool@ to a @'Status'@
--
-- >>> excludeIf True
-- >>> excludeIf False
-- Exclude
-- Include
excludeIf :: Bool -> Status
excludeIf True  = Exclude
excludeIf False = Include

-- | Represents failures in a @'CriterionThatCanFail'@.
type CriterionFailure =  Text

{- |
A type where the @Left@ branch is a @CriterionFailure@
and the @Right@ is a @Criterion@.
-}
type CriterionThatCanFail = Either CriterionFailure Criterion

-- | Smart constructor a @'Criterion'@.
makeCriterion :: Text -> Status -> Criterion
makeCriterion = curry MkCriterion

getStatus :: Criterion -> Status
getStatus (MkCriterion (_, s)) = s

getReason :: Criterion -> Text
getReason (MkCriterion (r, _)) = r

{-
INTERNAL
A collection of @'Criterion'@ paired with a @Natural@ number,
respresenting the order of each @'Criterion'@.
-}
newtype CriteriaI = MkCriteriaI [ (Natural, Criterion) ]
  deriving (Eq, Show)

{-|
A type where the @Left@ branch is a @CriterionFailure@
and the @Right@ is a @CriteriaI@.
-}
type Criteria = Either CriterionFailure CriteriaI

instance From [Criterion] Criteria where
  from = pure . criteriaI

-- | Constructs a @'Criteria'@ from a list of @'CriterionThatCanFail'@.
instance From [CriterionThatCanFail] Criteria where
  from x = fmap criteriaI (sequenceA x)

-- Unpacks a 'Criteria'.
getCriteria :: CriteriaI -> [(Natural, Criterion)]
getCriteria (MkCriteriaI x) = x

-- Constructs a @'Criteria'@ from a list of @'Criterion'@.
criteriaI :: [Criterion] -> CriteriaI
criteriaI l = MkCriteriaI $ zip [1 ..] l

{- |
A type which collects the counts of subjects included or excluded.
-}
data AttritionInfo = MkAttritionInfo
  { totalSubjectsProcessed :: Int
  , totalUnitsProcessed    :: Int
  , attritionInfo          :: Map CohortStatus Natural
  }
  deriving (Eq, Show, Generic)

instance ToJSON AttritionInfo
instance FromJSON AttritionInfo
instance Binary AttritionInfo

{-
Two @AttritionInfo@ values can be combined,
but this meant for combining
attrition info from the same set of @Criteria@.

When all @AttritionInfo@ have been created
using 'initAttritionInfo' from the same set of 'Criteria'
(i.e. the criteria contain the same exclusions),
then @AttritionInfo@ can be safely combined.
This will be the case within a cohort.
-}
instance Semigroup AttritionInfo where
  (<>) (MkAttritionInfo s1 u1 i1) (MkAttritionInfo s2 u2 i2) =
    MkAttritionInfo (s1 + s2) (u1 + u2) (unionsWith (+) [i1, i2])

instance Monoid AttritionInfo where
  mempty =
    MkAttritionInfo 0 0 (fromList [(SubjectHasNoIndex, 0), (Included, 0)])

{- |
Converts a unit's @'Criteria'@ to a @'CohortStatus'@.
The status is set to @'Included'@
if none of the @'Criterion'@ have a status of @'Exclude'@.
-}
checkCohortStatus :: Criteria -> CohortStatus
checkCohortStatus (Left  cf) = CriteriaFailure cf
checkCohortStatus (Right (MkCriteriaI xs)) = getStatus xs
  where
    op (i, MkCriterion (name, Exclude)) = Left (i, name)
    op (i, MkCriterion (name, Include)) = Right ()
    getStatus xs' = case traverse op xs' of
      Left (i, name) -> ExcludedBy (i, name)
      Right _        -> Included


-- Initializes @AttritionInfo@ from a @'Criteria'@.
initAttritionInfo :: Criteria -> Map.Map CohortStatus Natural
initAttritionInfo = \case
  Left e -> fromList [(CriteriaFailure e, 0)]
  Right x ->
    fromList $ zip (initStatusInfo x) (replicate (length $ getCriteria x) 0)
  where
    -- Initializes a container of @'CohortStatus'@ from a @'CriteriaI'@,
    --in order to generate all the possible Exclusion/Inclusion.
    initStatusInfo :: CriteriaI -> [CohortStatus]
    initStatusInfo x =
      fmap (ExcludedBy . Data.Bifunctor.second getReason) (getCriteria x)
        <> pure Included

{-|
Measures @'AttritionInfo'@ from a @'Criteria'@ and a list of @'CohortStatus'@
**for a single subject**.
The 'AttritionInfo' across subjects can obtains by summing
a list of 'AttritionInfo'.

A note on why this function takes 'Maybe Criteria' as input:
A subject may not have an 'Criteria'
if their only 'CohortStatus'is 'SubjectHasNoIndex.
However, a 'Criteria' is needed to initialize a 'Map.Map CohortStatus Natural'
with 'initAttritionInfo'.
-}
measureSubjectAttrition :: Maybe Criteria -> [CohortStatus] -> AttritionInfo
measureSubjectAttrition mcriteria statuses = MkAttritionInfo
    -- function is meant to be used on a single subject, so intiialize to one
  1
    -- number of units is number of statuses not equal to SubjectHasNoIndex
  (length $ filter (/= SubjectHasNoIndex) statuses)
    -- attritionInfo is formed by unioning via a Map in order to
    -- sum within each CohortStatus
  (unionsWith
    (+)
    [ maybe mempty initAttritionInfo mcriteria
    , Map.fromListWith (+) $ fmap (, 1) statuses
    ]
  )

{- |
**This function is a convenience function for writing tests.**

Do not use unless you know what you're doing.
-}
makeTestAttritionInfo
  :: Int -- ^ count of subjects
  -> Int -- ^ count of units
  -> [(CohortStatus, Natural)] -- ^ list of statuses with counts
  -> AttritionInfo
makeTestAttritionInfo x y z = MkAttritionInfo x y $ fromList z

{-|
Converts a @Feature n Status@ to a @Criterion@.
In the case that the value of the @'Features.Core.FeatureData'@ is @Left@,
the status is set to @'Exclude'@.
-}
instance KnownSymbol n => From (Feature n Status) CriterionThatCanFail where
  from x = case getData x of
    Left  e -> Left $ pack $ show e
    Right v -> Right (MkCriterion (pack $ symbolVal (Proxy @n), v))

