{-|
Module      : Cohort Criteria
Description : Defines the Criteria and related types and functions
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

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
import           Data.Map.Strict               as Map
                                                ( Map
                                                , fromListWith
                                                , unionsWith
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Features.Core                  ( Feature
                                                , getData
                                                )
import           GHC.Exts                       ( IsList(..) )
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( Natural )
import           GHC.TypeLits                   ( KnownSymbol
                                                , symbolVal
                                                )
import           Witch                          ( From(..) )
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
newtype Criterion = MkCriterion ( Text, Status ) deriving (Eq, Show)

-- | Converts a @'Feature'@ to a @'Criterion'@.
criterion :: Text -> Status -> Criterion
criterion = curry MkCriterion

getStatus :: Criterion -> Status
getStatus (MkCriterion (_, s)) = s

getReason :: Criterion -> Text
getReason (MkCriterion (r, _)) = r

{-|
Converts a @Feature n Status@ to a @Criterion@.
In the case that the value of the @'Features.Core.FeatureData'@ is @Left@,
the status is set to @'Exclude'@. 
-}
instance KnownSymbol n => From (Feature n Status) Criterion where
  from x = MkCriterion
    ( pack $ symbolVal (Proxy @n)
    , case s of
      Left  mr  -> Exclude
      Right sta -> sta
    )
    where s = getData x


-- | A nonempty collection of @'Criterion'@ paired with a @Natural@ number.
newtype Criteria = MkCriteria [ (Natural, Criterion) ]
  deriving (Eq, Show)

-- | Unpacks a 'Criteria'.
getCriteria :: Criteria -> [(Natural, Criterion)]
getCriteria (MkCriteria x) = x

-- | Constructs a @'Criteria'@ from a list of @'Criterion'@.
criteria :: [Criterion] -> Criteria
criteria l = MkCriteria $ zip [1 ..] l

-- | Converts a subject's @'Criteria'@ into a list of triples of 
-- (order of criterion, name of criterion, status)
getStatuses :: Criteria -> [(Natural, Text, Status)]
getStatuses (MkCriteria x) =
  fmap (\c -> (fst c, (getReason . snd) c, (getStatus . snd) c)) x

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

{-|
Initializes a container of @'CohortStatus'@ from a @'Criteria'@. 
This can be used to generate all the possible Exclusion/Inclusion reasons.
-}
initStatusInfo :: Criteria -> [CohortStatus]
initStatusInfo (MkCriteria z) =
  fmap (ExcludedBy . Data.Bifunctor.second getReason) z <> pure Included


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

instance Monoid AttritionInfo where
  mempty =
    MkAttritionInfo 0 0 (fromList [(SubjectHasNoIndex, 0), (Included, 0)])

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
