{-|
Module      : Cohort.Core
Description : Internal module defining logic to process a list of @Subject@
              into a @Cohort@.
Copyright   : (c) Target RWE 2023
License     : BSD3
Maintainer  : bbrown@targetrwe.com 
              ljackman@targetrwe.com 
              dpritchard@targetrwe.com

This is module supplies the logic for how cohorts are processed based on the
user-defined @CohortSpecMap@. It is intended to be used internally to
`hasklepias-main` and not re-exported from there.

For high-level documentation of the cohort-processing pipeline, see the
`Hasklepias` module documentation from `hasklepias-main`.

Command-line options, defined in `hasklepias-main`, should not affect the
internal logic of cohort processing. Only the CohortSpec should be able to do so.

Note 'subjData' is processed *only* via the logic provided in 'CohortSpec'.
-}

module Cohort.Core where

import           Cohort.Cohort
import           Cohort.Criteria    (Criterion (..), firstExclude)
import qualified Cohort.IndexSet    as IS
import           Data.Foldable      (foldl')
import qualified Data.Map.Strict    as M
import           Data.Text          (Text)
import           EventDataTheory    (Interval)

{- Subject-level evaluation -}

-- | Evaluate a subject at a single index time, producing @Left@ if there were
-- any @Exclude@ among the computed @Criteria@, and otherwise returning @Right
-- (ObsUnit b)@ with variables computed via @runVariables@.
evalSubjAtIndex :: CohortSpec t m a -> Subject t m a -> Interval a -> Either (ObsId a, Text) (ObsUnit a)
evalSubjAtIndex spec subj i = maybe (Right unit) (\c -> Left (obsid, statusLabel c)) $ firstExclude crits
  where
    sid = subjId subj
    dt = subjData subj
    obsid = MkObsId sid i
    crits = runCriteria spec dt i
    -- NOTE this step is expensive if runVariables is expensive
    unit = MkObsUnit obsid $ runVariables spec dt i

-- | Compute @IndexSet@, @Criteria@ and variables for a given subject via
-- strict fold on the @IndexSet@. Returns @SNoIndex@ if and only if @IS.'null'
-- idxs@. As reflected in the definition of @EvaluatedSubject@, @runVariables@
-- is computed only for units whose @Criteria@ have @status@ @Include@. This
-- also updates the @attritionSubj@, the @AttritionInfo@ associated with this
-- subject.
evalSubj :: CohortSpec t m a -> Subject t m a -> EvaluatedSubject a
evalSubj spec subj = IS.foldl' op (SNoIndex (subjId subj)) idxs
  where
        idxs = runIndices spec (subjData subj)
        -- Single-subject attrition. Accumulates units processed and attrition
        -- by status.
        initAttrition = emptyAttrition{ subjectsProcessed = 1 }
        updateExcl (oid, t) ess iss attr = SUnits $ MkSUnitData ((oid, t):ess) iss $
                                      incrementAttritionForUnit (ExcludedBy t) attr
        updateIncl x ess iss attr = SUnits $ MkSUnitData ess (x:iss) $
                                      incrementAttritionForUnit Included attr
        op (SNoIndex _) i = case evalSubjAtIndex spec subj i of
                              Left x  -> updateExcl x [] [] initAttrition
                              Right x -> updateIncl x [] [] initAttrition
        op (SUnits (MkSUnitData ess iss attr)) i = case evalSubjAtIndex spec subj i of
                                  Left x  -> updateExcl x ess iss attr
                                  Right x -> updateIncl x ess iss attr


-- | Processes the cohort-building logic provided in @CohortSpec@ on a list of
-- @Subject@s. @Subject@s are processes one-by-one, and @AttritionInfo@ is
-- accumulated as each subject is processed. @Subject@s whose set of index
-- times is empty are counted only in the @subjectsProcessed@ field of
-- @AttritionInfo@.
--
-- Each @Subject@ produces one @ObsUnit@ in the @Cohort@ for each element of
-- the subject's @IndexSet@ such that all @Criteria@ associated with that
-- index time have status @Include@. If at least one element of the @Criteria@
-- for an index time has status @Exclude@, no variables are computed and the
-- exclusion is recorded in the @AttritionInfo@.
evalCohort :: CohortSpec t m a -> [Subject t m a] -> Cohort a
evalCohort spec ss = foldl' op emptyCohort (map (evalSubj spec) ss)
  where
    -- SNoIndex is counted for subjects processed but nothing more.
    emptySubjAttrition = emptyAttrition{ subjectsProcessed = 1 }
    op (MkCohort a us) (SNoIndex _) = MkCohort (combineAttrition a emptySubjAttrition) us
    -- NOTE: this concat is a good place to look for efficiency gains if needed.
    -- us1 ++ us2 preserves order of subjects.
    op (MkCohort a1 us1) (SUnits (MkSUnitData _ us2 a2)) = MkCohort (combineAttrition a1 a2) (us1 ++ us2)


-- | A convenience function to apply @evalCohort@ to a fixed list of subjects,
-- once for each element of a @CohortMapSpec@. Each element of the
-- @CohortMapSpec@ represents a different, user-provided cohort-building
-- logic.
evalCohortMap :: CohortSpecMap t m a -> [Subject t m a] -> CohortMap a
evalCohortMap specm subj = M.map (`evalCohort` subj) specm
