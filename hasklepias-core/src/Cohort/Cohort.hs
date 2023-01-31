{-|
Module      : Cohort.Cohort
Description : Defines the Cohort and related types used in core logic of
              producing a Cohort from subject-level data.
Copyright   : (c) Target RWE 2023
License     : BSD3
Maintainer  : bbrown@targetrwe.com 
              ljackman@targetrwe.com 
              dpritchard@targetrwe.com
-}

{-# LANGUAGE DeriveGeneric #-}

module Cohort.Cohort where

import           Cohort.Criteria     (Criteria)
import           Cohort.IndexSet     (IndexSet)
import           Data.Aeson          (ToJSON, ToJSONKey)
import           Data.List           (sortOn)
import           Data.List.NonEmpty  (NonEmpty (..), (<|))
import           Data.Map.Strict     (Map, alter, empty, unionWith)
import           Data.Text           (Text)
import           EventDataTheory     (Event, Interval)
import           Features.Featureset (Featureset)
import           GHC.Generics        (Generic)

{- COHORT SPECIFICATION -}

-- | A container for all logic needed to construct a cohort. The provided
-- functions will be run on subject-level data, subject-by-subject. Logic will
-- be executed as part of a @'CohortApp'@ pipeline. The type 'CohortApp' itself
-- is not exported, but the user will create such a pipeline via the exported
-- 'cohortMain'. 
--
-- Note 'runVariables' must construct *all* output variables. For a more
-- detailed explanation of the cohort-building pipeline, see the `Hasklepias`
-- module documentation from `hasklepias-main`.
data CohortSpec t m a
  = MkCohortSpec
      { runIndices   :: NonEmpty (Event t m a) -> IndexSet a
        -- ^ Constructs the set of index times, of type @Interval a@,
        -- for a given subject based on the full list of subject data in
        -- the 'NonEmpty' input list of @Event t m a@.
      , runCriteria  :: NonEmpty (Event t m a) -> Interval a -> Criteria
        -- ^ Constructs a non-empty list of inclusion / exclusion
        -- criteria for a single subject, relative to a particular index
        -- time. There will be one 'Criteria' value for each subject and
        -- element of @'IndexSet' a@.
      , runVariables :: NonEmpty (Event t m a) -> Interval a -> Featureset
        -- ^ Construct all output variables for the provided subject
        -- data, with respect to a particular index time.
      }

-- | Container for @CohortSpec@ for different cohorts, as given by the @Text@
-- keys.
type CohortSpecMap t m a = Map Text (CohortSpec t m a)

{- COHORT PIPELINE INPUT -}

-- | Wrapper for a @Text@ subject identifier. The wrapper allows us to
-- guarantee certain properties for subject identifiers, instead of allowing
-- arbitrary @Text@.
newtype SubjId
  = MkSubjId Text
  deriving (Eq, Generic, Ord, Show)

instance ToJSON SubjId

-- | Subject-level data. The API via CohortSpec allows programmers to
-- manipulate contents of 'subjData' only, not 'subjId'. Attempting to process
-- a 'Subject' without any events is ill-defined, hence the use of 'NonEmpty'.
data Subject t m a
  = MkSubject
      { subjId   :: SubjId
      , subjData :: NonEmpty (Event t m a)
      }
  deriving (Eq, Show)

{- COHORT PIPELINE OUTPUT -}

-- | Internal. Status used for final inclusion/exclusion and the key of
-- 'attritionByStatus'. An observational unit can have multiple 'Exclude'
-- values but only be associated with a single 'ExcludeBY'.
data AttritionStatus
  = Included
  | ExcludedBy Text
  deriving (Eq, Generic, Ord, Show)

instance ToJSON AttritionStatus
instance ToJSONKey AttritionStatus

-- | Container for accumulated inclusion/exclusion counts.
data AttritionInfo
  = MkAttritionInfo
      { subjectsProcessed :: Int
      , unitsProcessed    :: Int
      , attritionByStatus :: Map AttritionStatus Int
      }
  deriving (Eq, Generic, Show)

instance ToJSON AttritionInfo

-- | Identifier for the unit element of a Cohort, @ObsUnit@. @fromSubjId@
-- should refer to the @subjId@ of the @Subject@ from which an @ObsUnit@ was
-- constructed. There should be one @ObsId@, and hence one @ObsUnit@, per pair
-- formed from a given @Subject@ and each element of the subject's @IndexSet@.
-- @indexTime@ gives the element of @IndexSet@ associated with this @ObsUnit@.
data ObsId a
  = MkObsId
      { fromSubjId :: SubjId
      , indexTime  :: Interval a
      }
  deriving (Eq, Generic, Ord, Show)

instance (ToJSON a) => ToJSON (ObsId a)

-- | There should be one @ObsUnit@ per pair formed from a given @Subject@ and
-- each element of the subject's @IndexSet@.
data ObsUnit a
  = MkObsUnit
      { obsId   :: ObsId a
      , obsData :: Featureset
      }
  deriving (Show)

-- TODO it should be an error that a subject does not have an index date. such
-- cases were dropped, both previously and in the current implementation. an
-- error-handling redesign should address that.

-- | Internal type to hold the result of @Cohort.Core.'evaluateSubj'@. It
-- should not be used for any other purpose.
data EvaluatedSubject a
  = SNoIndex SubjId
  -- ^ @Subject@ had no index. These contribute to the subjectsProcessed count
  -- but are silently dropped.
  | SUnits (SUnitData a)
  deriving (Show)

-- | Internal. Holds data for 'SUnits'.
data SUnitData a
  = MkSUnitData
      { excludeUnits  :: [(ObsId a, Text)]
        -- ^ Observational units that are to be excluded from the cohort.
        -- Only @ObsId@ is retained since no @obsData@ is to be computed.
        -- The @Text@ should be the @statusLabel@ field's value of
        -- @Criterion@.
      , includeUnits  :: [ObsUnit a]
        -- ^ Observational units to be included in the cohort, on which
        -- @runVariables@ is to be computed.
      , attritionSubj :: AttritionInfo
        -- ^ Subject-level attrition, to allow computing attritionInfo as each
        -- subject is processed.
      }
  deriving (Show)

-- | Accumulated data from a single list of subjects, processed according to
-- the logic provided in a @CohortSpec@. It is the output type of @evalCohort@
-- and should be produced nowhere else.
data Cohort a
  = MkCohort
      { attritionInfo :: AttritionInfo
      , cohortData    :: [ObsUnit a]
      }
  deriving (Show)

type CohortMap a = Map Text (Cohort a)

{- EXPORTED UTILITIES -}

-- NOTE this does not use Data.Map.fromListWith or similar because it should be
-- similar in complexity but avoids the (unlikely) issue related to map size
-- mentioned in that module. Should be evalated in an optimization pass.

-- TODO as part of a future redesign providing error-handling, this function
-- should incorporate some minimal validation of subject ids, which possibly is
-- configurable.

-- | Accumulate @[(Text, Event t m a)]@ into @[Subject t m a]@. The former is
-- the \'flat\' output format produced in 'EventDataTheory.parseEventLinesL'.
eventsToSubject :: [(Text, Event t m a)] -> [Subject t m a]
eventsToSubject [] = []
eventsToSubject es = foldr op [] es'
  where es' = sortOn fst es
        op (sid', e') [] = [MkSubject (MkSubjId sid') (e' :| [])]
        op (sid', e') (subj@(MkSubject (MkSubjId sid) ess) : ss)
          | sid == sid' = MkSubject (MkSubjId sid) (e' <| ess) : ss
          | otherwise = MkSubject (MkSubjId sid') (e' :| []) : subj : ss


{- INTERNAL UTILITIES -}

emptyCohort :: Cohort b
emptyCohort = MkCohort emptyAttrition []

emptyAttrition :: AttritionInfo
emptyAttrition = MkAttritionInfo 0 0 empty

alterAttrition :: AttritionStatus -> Map AttritionStatus Int -> Map AttritionStatus Int
alterAttrition = alter op
  where
        -- If key `k` doesn't exist, insert it with value 1.
        -- Else increment by 1.
        op Nothing  = Just 1
        op (Just n) = Just (n+1)

-- | Internal. Increment AttritionInfo with a single unit's information. Does
-- not alter @subjectsProcessed@.
incrementAttritionForUnit :: AttritionStatus -> AttritionInfo -> AttritionInfo
incrementAttritionForUnit s info = info{ unitsProcessed = 1 + unitsProcessed info, attritionByStatus = alterAttrition s (attritionByStatus info) }

-- | Combine two @AttritionInfo@ values by summing the counts, and taking the
-- union of @attritionByStatus@, summing values for shared keys.
combineAttrition :: AttritionInfo -> AttritionInfo -> AttritionInfo
combineAttrition a1 a2 = MkAttritionInfo { subjectsProcessed = sp, unitsProcessed = up, attritionByStatus = m }
  where
    sp = subjectsProcessed a1 + subjectsProcessed a2
    up = unitsProcessed a1 + unitsProcessed a2
    m = unionWith (+) (attritionByStatus a1) (attritionByStatus a2)
