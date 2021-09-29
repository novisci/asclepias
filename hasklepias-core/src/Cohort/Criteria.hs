{-|
Module      : Cohort Criteria
Description : Defines the Criteria and related types and functions
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# OPTIONS_HADDOCK hide #-}
-- {-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}


module Cohort.Criteria
  ( Criterion
  , Criteria(..)
  , Status(..)
  , CohortStatus(..)
  , criterion
  , criteria
  , excludeIf
  , includeIf
  , initStatusInfo
  , checkCohortStatus
  ) where

import           Control.Applicative            ( Applicative(pure) )
import           Control.Monad                  ( Functor(..) )
import           Data.Aeson                     ( (.=)
                                                , ToJSON(..)
                                                , object
                                                )
import           Data.Bifunctor                 ( Bifunctor(second) )
import           Data.Bool                      ( (&&)
                                                , Bool(..)
                                                , not
                                                , otherwise
                                                )
import           Data.Either                    ( either )
import           Data.Eq                        ( Eq(..) )
import           Data.Function                  ( ($)
                                                , (.)
                                                , const
                                                , id
                                                )
import           Data.List                      ( (++)
                                                , find
                                                )
import qualified Data.List.NonEmpty            as NE
                                                ( NonEmpty
                                                , fromList
                                                , zip
                                                )
import           Data.Maybe                     ( Maybe(..)
                                                , maybe
                                                )
import           Data.Ord                       ( Ord(..)
                                                , Ordering(..)
                                                )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Tuple                     ( fst
                                                , snd
                                                )
import           Features.Compose               ( Feature
                                                , FeatureN(..)
                                                , getFeatureData
                                                , nameFeature
                                                )
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( Natural
                                                , Num((+))
                                                )
import           GHC.Show                       ( Show(show) )
import           GHC.TypeLits                   ( KnownSymbol
                                                , symbolVal
                                                )

import Cohort.Index
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

-- Defines an ordering to put @SubjectHasNoIndex@ first and @Included@ last. 
-- The @'ExcludedBy'@ are ordered by their number value.
instance Ord CohortStatus where
  compare Included            Included            = EQ
  compare SubjectHasNoIndex   SubjectHasNoIndex   = EQ
  compare Included            (ExcludedBy _)      = GT
  compare (ExcludedBy _ )     Included            = LT
  compare Included            SubjectHasNoIndex   = LT
  compare SubjectHasNoIndex   Included            = GT
  compare (ExcludedBy _  )    SubjectHasNoIndex   = LT
  compare SubjectHasNoIndex   (ExcludedBy _  )    = GT
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
newtype Criteria = MkCriteria {
    getCriteria :: NE.NonEmpty (Natural, Criterion)
  } deriving (Eq, Show)

-- | Constructs a @'Criteria'@ from a @'NE.NonEmpty'@ collection of @'Criterion'@.
criteria :: NE.NonEmpty Criterion -> Criteria
criteria l = MkCriteria $ NE.zip (NE.fromList [1 ..]) l

-- | Unpacks a @'Criterion'@ into a (Text, Status) pair where the text is the
-- name of the criterion and its @Status@ is the value of the status in the 
-- @'Criterion'@. In the case, that the value of the @'Features.Compose.FeatureData'@ 
-- within the @'Criterion'@ is @Left@, the status is set to @'Exclude'@. 
getStatus :: Criterion -> (Text, Status)
getStatus (MkCriterion x) = either (const (nm, Exclude))
                                   (nm, )
                                   ((getFeatureData . getDataN) x)
  where nm = getNameN x

-- | Converts a subject's @'Criteria'@ into a @'NE.NonEmpty'@ triple of 
-- (order of criterion, name of criterion, status)
getStatuses :: Criteria -> NE.NonEmpty (Natural, Text, Status)
getStatuses (MkCriteria x) =
  fmap (\c -> (fst c, (fst . getStatus . snd) c, (snd . getStatus . snd) c)) x

-- | An internal function used to @'Data.List.find'@ excluded statuses. Used in
-- 'checkCohortStatus'.
findExclude :: Criteria -> Maybe (Natural, Text, Status)
findExclude x = find (\(_, _, z) -> z == Exclude) (getStatuses x)

-- | Converts a subject's @'Criteria'@ to a @'CohortStatus'@. The status is set
-- to @'Included'@ if none of the @'Criterion'@ have a status of @'Exclude'@.
checkCohortStatus :: Maybe (Index i a) -> Criteria -> CohortStatus
checkCohortStatus Nothing _  = SubjectHasNoIndex 
checkCohortStatus (Just index) x =
  maybe Included (\(i, n, _) -> ExcludedBy (i, n)) (findExclude x)

-- | Utility to get the name of a @'Criterion'@.
getCriterionName :: Criterion -> Text
getCriterionName (MkCriterion x) = getNameN x

-- | Initializes a container of @'CohortStatus'@ from a @'Criteria'@. This can be used
-- to collect generate all the possible Exclusion/Inclusion reasons. 
initStatusInfo :: Criteria -> NE.NonEmpty CohortStatus
initStatusInfo (MkCriteria z) =
  fmap (ExcludedBy . Data.Bifunctor.second getCriterionName) z <> pure Included
