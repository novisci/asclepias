{-|
Module      : Cohort.Criteria
Description : Types and utilities related to @Include@ and @Exclude@ statuses
              for a given cohort.
Copyright   : (c) Target RWE 2022
License     : BSD3
Maintainer  : bbrown@targetrwe.com, ljackman@targetrwe.com, dpritchard@targetrwe.com

This module defines the return type of @Cohort.Cohort.'runCriteria'@ in a
@CohortSpec@. @Criteria@, an alias for @NonEmpty Criterion@, is computed
subject-by-subject for each cohort. Each @Criteria@ is computed from
subject-level data and a given index time. A single subject has one value of
@Criteria@, with one or more @Criterion@ values, at each given index time. 

Each @Criterion@ indicates whether an @Cohort.Cohort.'ObsUnit'@, of which there
is one per subject and index time, should be @Exclude@ed from the cohort, or
@Include@ed contingent on the @Cohort.Cohort.'ObsUnit'@ not being @Exclude@ed
by any other criterion. In other words, a @Cohort.Cohort.'ObsUnit'@ is included
if and only if they are @Include@ed for every @Criterion@.

See the `Hasklepias` module documentation in `hasklepias-main` for more
information on how @Criteria@ are processed in a @CohortApp@ pipeline.
-}

module Cohort.Criteria
  ( Criterion(..)
  , Status(..)
  , Criteria
  , firstExclude
  , includeIf
  , excludeIf
  ) where

import           Control.Applicative (asum)
import qualified Data.List.NonEmpty  as NE
import           Data.Text           (Text)

{- TYPES -}

-- | Indication of whether a given subject should be included or excluded from
-- the cohort.
data Status = Include | Exclude deriving (Eq, Show)

-- | A @Status@ paired with some descriptive label.
data Criterion
  = MkCriterion
      { statusLabel :: Text
      , status      :: Status
      }
  deriving (Eq, Show)

-- | Type alias for a @NE.'NonEmpty'@ list of @Criterion@. This is the return
-- type of @Cohort.Cohort.CohortSpec.'runCriteria'@. @NE.'NonEmpty'@ is used to
-- prevent ill-defined behavior in the cohort processing logic of
-- @Cohort.Core@: An subject with no @Criterion@ computed for a given index
-- time would need to be given an implicit @Include@ or @Exclude@ with empty
-- label, which instead is better supplied explicitly as a singleton list of
-- @Criteria@.
type Criteria = NE.NonEmpty Criterion


{- EXPORTED UTILITIES -}

-- | Helpers to convert a @Bool@ to a @'Status'@.
--
-- >>> includeIf True
-- >>> includeIf False
-- Include
-- Exclude
includeIf, excludeIf :: Bool -> Status
includeIf True  = Include
includeIf False = Exclude

excludeIf True  = Exclude
excludeIf False = Include


{- INTERNAL UTILITIES -}

-- NOTE: By using NE.map, you protect against a subtle bug in which Criteria
-- is changed to a possibly empty container. asum on a list of Maybe will
-- return Nothing on the empty list, which in current usage downstream would
-- imply a default of Include when a subject had no Criteria. 
firstExclude :: Criteria -> Maybe Criterion
firstExclude = asum . NE.map op
  where op (MkCriterion lab Exclude) = Just (MkCriterion lab Exclude)
        op _                         = Nothing
