{-|
Module      : Cohort Criteria
Description : Defines the Criteria and related types and functions
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Cohort.Criteria
  ( Criterion
  , Criteria
  , Status(..)
  , CohortStatus(..)
  , criterion
  , criteria
  , excludeIf
  , includeIf
  , checkCohortStatus
  , AttritionInfo
  , measureSubjectAttrition
  , makeTestAttritionInfo
  ) where

import           Data.Aeson                     ( (.=)
                                                , FromJSON(..)
                                                , FromJSONKey(..)
                                                , ToJSON(..)
                                                , ToJSONKey(..)
                                                , object
                                                )
import           Data.Bifunctor                 ( Bifunctor(second) )
import           Data.List                      ( find )
import qualified Data.List.NonEmpty            as NE
                                                ( NonEmpty
                                                , fromList
                                                , zip
                                                )
import           Data.Map.Strict               as Map
                                                ( Map
                                                , fromListWith
                                                , unionsWith
                                                )
import qualified Data.Set                      as Set
                                                ( Set )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Features.Core                  ( Feature
                                                , FeatureN(..)
                                                , getFeatureData
                                                , nameFeature
                                                )
import           GHC.Exts                       ( IsList(..) )
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( Natural )
import           GHC.TypeLits                   ( KnownSymbol
                                                , symbolVal
                                                )

-- | Defines the return type for @'Criterion'@ indicating whether to include or 
-- exclude a subject.
data Status = Include | Exclude deriving (Eq, Show, Generic)

-- | Defines subject's diposition in a cohort either included or which criterion
-- they were excluded by. See @'checkCohortStatus'@ for evaluating a @'Criteria'@
-- to determine CohortStatus.
data CohortStatus =
    SubjectHasNoIndex
  | ExcludedBy (Natural, Text)
  | Included
    deriving (Eq, Show, Generic)

instance ToJSON CohortStatus where
instance FromJSON CohortStatus where
instance ToJSONKey CohortStatus
instance FromJSONKey CohortStatus

-- Defines an ordering to put @SubjectHasNoIndex@ first and @Included@ last. 
-- The @'ExcludedBy'@ are ordered by their number value.
instance Ord CohortStatus where
  compare Included            Included            = EQ
  compare SubjectHasNoIndex   SubjectHasNoIndex   = EQ
  compare Included            (ExcludedBy _)      = GT
  compare (ExcludedBy _)      Included            = LT
  compare Included            SubjectHasNoIndex   = GT
  compare SubjectHasNoIndex   Included            = LT
  compare (ExcludedBy _)      SubjectHasNoIndex   = GT
  compare SubjectHasNoIndex   (ExcludedBy _     ) = LT
  compare (ExcludedBy (i, _)) (ExcludedBy (j, _)) = compare i j

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

-- | A type that is simply a @'FeatureN Status'@, that is, a feature that 
-- identifies whether to @'Include'@ or @'Exclude'@ a subject.
newtype Criterion = MkCriterion ( FeatureN Status ) deriving (Eq, Show)

-- | Converts a @'Feature'@ to a @'Criterion'@.
criterion :: (KnownSymbol n) => Feature n Status -> Criterion
criterion x = MkCriterion (nameFeature x)

-- | A nonempty collection of @'Criterion'@ paired with a @Natural@ number.
newtype Criteria = MkCriteria ( NE.NonEmpty (Natural, Criterion) )
  deriving (Eq, Show)

-- | Unpacks a 'Criteria'.
getCriteria :: Criteria -> NE.NonEmpty (Natural, Criterion)
getCriteria (MkCriteria x) = x

-- | Constructs a @'Criteria'@ from a @'NE.NonEmpty'@ collection of @'Criterion'@.
criteria :: NE.NonEmpty Criterion -> Criteria
criteria l = MkCriteria $ NE.zip (NE.fromList [1 ..]) l

-- | Unpacks a @'Criterion'@ into a (Text, Status) pair where the text is the
-- name of the criterion and its @Status@ is the value of the status in the 
-- @'Criterion'@. In the case, that the value of the @'Features.Core.FeatureData'@ 
-- within the @'Criterion'@ is @Left@, the status is set to @'Exclude'@. 
getStatus :: Criterion -> (Text, Status)
getStatus (MkCriterion x) =
  (\case
      Left  _ -> (nm, Exclude)
      Right v -> (nm, v)
    )
    ((getFeatureData . getDataN) x)
  where nm = getNameN x

-- | Converts a subject's @'Criteria'@ into a @'NE.NonEmpty'@ triple of 
-- (order of criterion, name of criterion, status)
getStatuses :: Criteria -> NE.NonEmpty (Natural, Text, Status)
getStatuses (MkCriteria x) =
  fmap (\c -> (fst c, (fst . getStatus . snd) c, (snd . getStatus . snd) c)) x

{-|
An internal function used to @'Data.List.find'@ excluded statuses. 
Used in 'checkCohortStatus'
-}
findExclude :: Criteria -> Maybe (Natural, Text, Status)
findExclude x = find (\(_, _, z) -> z == Exclude) (getStatuses x)

{-|
Converts a subject's @'Criteria'@ to a @'CohortStatus'@.
The status is set to @'Included'@
if none of the @'Criterion'@ have a status of @'Exclude'@.
-}
checkCohortStatus :: i -> Criteria -> CohortStatus
checkCohortStatus index x =
  maybe Included (\(i, n, _) -> ExcludedBy (i, n)) (findExclude x)

-- | Utility to get the @Text@ name of a @'Criterion'@.
getCriterionName :: Criterion -> Text
getCriterionName (MkCriterion x) = getNameN x

{-|
Initializes a container of @'CohortStatus'@ from a @'Criteria'@. 
This can be used to generate all the possible Exclusion/Inclusion reasons.
-}
initStatusInfo :: Criteria -> NE.NonEmpty CohortStatus
initStatusInfo (MkCriteria z) =
  fmap (ExcludedBy . Data.Bifunctor.second getCriterionName) z <> pure Included


{- |
A type which collects the counts of subjects included or excluded.
-}
data AttritionInfo = MkAttritionInfo
  { totalSubjectsProcessed :: Int
  , totalUnitsProcessed    :: Int
  , attritionInfo          :: Map CohortStatus Natural
  }
  deriving (Eq, Show, Generic)


instance ToJSON AttritionInfo where
instance FromJSON AttritionInfo where

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

-- Initializes @AttritionInfo@ from a @'Criteria'@.
initAttritionInfo :: Criteria -> Map.Map CohortStatus Natural
initAttritionInfo x = fromList
  $ zip (toList (initStatusInfo x)) (replicate (length (getCriteria x)) 0)

{- |
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
    -- again, function is meant to be used on a single subject, so one
  1
    -- number of units is number of statuses not equal to SubjectHasNoIndex 
  (length $ filter (/= SubjectHasNoIndex) statuses)
    -- attritionInfo is formed by unioning via a Map in order to 
    -- sum within each CohortStatus
  (unionsWith
    (+)
    [ maybe mempty initAttritionInfo mcriteria
    , Map.fromListWith (+) $ fmap (, 1) statuses
    , fromList [(SubjectHasNoIndex, 0)]
    , fromList [(Included, 0)]
      -- including SubjectHasNoIndex and Included for the cases that none of the
      -- evaluated criteria have either of those statuses
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
