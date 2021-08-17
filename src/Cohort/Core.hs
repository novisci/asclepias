{-|
Module      : Hasklepias Cohorts
Description : Defines the Cohort type and associated methods
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE Safe #-}

module Cohort.Core(
      Subject(..)
    , ID
    , Population(..)
    , ObsUnit(..)
    , CohortData(..)
    , Cohort(..)
    , CohortSpec
    , AttritionInfo(..)
    , specifyCohort
    , makeObsUnitFeatures
    , evalCohort
    , getCohortIDs
    , getCohortData
    , getAttritionInfo
) where

import GHC.Num                              ( Num((+)), Natural )
import Data.Aeson                           ( FromJSON, ToJSON, ToJSONKey )
import Data.Bool                            ( Bool )
import Data.Eq                              ( Eq )
import Data.Foldable                        ( Foldable(length) )
import Data.Function                        ( ($) )
import Data.Functor                         ( Functor(fmap) )
import Data.Maybe                           ( Maybe(..), catMaybes )
import Data.List                            ( zipWith, replicate )
import Data.List.NonEmpty                   ( NonEmpty(..), zip, fromList, nonEmpty )
import Data.Map.Strict as Map               ( toList, fromListWith )
import Data.Text                            ( Text )
import GHC.Generics                         ( Generic )
import GHC.Show                             ( Show(..) )
import Cohort.Index                         ( makeIndex, Index(..) )
import Cohort.Criteria

-- | A subject identifier. Currently, simply @Text@.
type ID = Text

-- | A subject is just a pair of @ID@ and data.
newtype Subject d = MkSubject (ID, d)
    deriving (Eq, Show, Generic)

instance Functor Subject where
    fmap f (MkSubject (id, x)) = MkSubject (id, f x)

instance (FromJSON d) => FromJSON (Subject d) where

-- | A population is a list of @'Subject'@s
newtype Population d = MkPopulation  [Subject d] 
    deriving (Eq, Show, Generic)

instance Functor Population where
    fmap f (MkPopulation x) = MkPopulation (fmap (fmap f) x)

instance (FromJSON d) => FromJSON (Population d) where

-- | An observational unit is what a subject may be transformed into.
data ObsUnit d = MkObsUnit {
        obsID :: ID
      , obsData :: d }
    deriving (Eq, Show, Generic)

-- | A container for CohortData
newtype CohortData d = MkCohortData { getObsData :: [ObsUnit d] }
    deriving (Eq, Show, Generic)

-- | A cohort is a list of observational units along with @'AttritionInfo'@ 
-- regarding the number of subjects excluded by the @'Criteria'@. 
newtype Cohort d = MkCohort (Maybe AttritionInfo, CohortData d)
    deriving (Eq, Show, Generic)

-- | Gets the attrition info from a cohort
getAttritionInfo :: Cohort d -> Maybe AttritionInfo
getAttritionInfo (MkCohort (x, _)) = x

-- | Unpacks a @'Population'@ to a list of subjects.
getPopulation :: Population d -> [Subject d]
getPopulation (MkPopulation x) = x

-- | Gets the data out of  a @'Subject'@.
getSubjectData :: Subject d -> d
getSubjectData (MkSubject (_, x)) = x

-- | Tranforms a @'Subject'@ into a @'ObsUnit'@.
makeObsUnitFeatures :: (d1 -> d0) -> Subject d1 -> ObsUnit d0
makeObsUnitFeatures f (MkSubject (id, dat)) = MkObsUnit id (f dat)

-- | A cohort specification consist of two functions: one that transforms a subject's
-- input data into a @'Criteria'@ and another that transforms a subject's input data
-- into the desired return type.
data CohortSpec d1 d0 = MkCohortSpec
        { runCriteria:: d1 -> Criteria
        -- (Feature (Index i a))
        , runFeatures:: d1 -> d0 }

-- | Creates a @'CohortSpec'@.
specifyCohort :: (d1 -> Criteria) -> (d1 -> d0) ->  CohortSpec d1 d0
specifyCohort = MkCohortSpec

-- | Evaluates the @'runCriteria'@ of a @'CohortSpec'@ on a @'Population'@ to 
-- return a list of @Subject Criteria@ (one per subject in the population). 
evalCriteria :: CohortSpec d1 d0 -> Population d1 -> [Subject Criteria]
evalCriteria (MkCohortSpec runCrit _) (MkPopulation pop) = fmap (fmap runCrit) pop

-- | Convert a list of @Subject Criteria@ into a list of @Subject CohortStatus@
evalCohortStatus :: [Subject Criteria] -> [Subject CohortStatus]
evalCohortStatus = fmap (fmap checkCohortStatus)

-- | Runs the input function which transforms a subject into an observational unit. 
-- If the subeject is excluded, the result is @Nothing@; otherwise it is @Just@ 
-- an observational unit.
evalSubjectCohort :: (d1 -> d0) -> Subject CohortStatus -> Subject d1 -> Maybe (ObsUnit d0)
evalSubjectCohort f (MkSubject (id, status)) subjData =
    case status of
        Included     -> Just $ makeObsUnitFeatures f subjData
        ExcludedBy _ -> Nothing

-- | A type which collects the counts of subjects included or excluded.
newtype AttritionInfo = MkAttritionInfo (NonEmpty (CohortStatus, Natural))
    deriving (Eq, Show, Generic)

-- | Initializes @AttritionInfo@ from a @'Criteria'@.
initAttritionInfo :: Criteria -> AttritionInfo
initAttritionInfo x =
    MkAttritionInfo $ zip (initStatusInfo x) 
        (0 :| replicate (length (getCriteria x)) 0)

-- | Creates an @'AttritionInfo'@ from a list of @Subject CohortStatus@. The result
-- is @Nothing@ if the input list is empty.
measureAttrition :: [Subject CohortStatus] -> Maybe AttritionInfo
measureAttrition l = fmap MkAttritionInfo $ nonEmpty $ Map.toList $
     Map.fromListWith (+) $ fmap (\x -> (getSubjectData x, 1)) l

-- | The internal function to evaluate a @'CohortSpec'@ on a @'Population'@. 
evalUnits :: CohortSpec d1 d0 -> Population d1 -> (Maybe AttritionInfo, CohortData d0)
evalUnits spec pop =
    ( measureAttrition statuses
    , MkCohortData $ catMaybes $ zipWith (evalSubjectCohort (runFeatures spec))
                                statuses
                                (getPopulation pop))
    where crits = evalCriteria spec pop
          statuses = evalCohortStatus crits

-- | Evaluates a @'CohortSpec'@ on a @'Population'@.
evalCohort :: CohortSpec d1 d0 -> Population d1 -> Cohort d0
evalCohort s p = MkCohort $ evalUnits s p

-- | Get IDs from a cohort.
getCohortIDs :: Cohort d -> [ID]
getCohortIDs (MkCohort (_, dat)) = fmap obsID ( getObsData dat )

-- | Get data from a cohort.
getCohortData :: Cohort d -> [d]
getCohortData (MkCohort (_, dat)) = fmap obsData ( getObsData dat )