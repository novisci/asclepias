{-|
Module      : Hasklepias Subject Type
Description : Defines the Subject type
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE Safe #-}

module Hasklepias.Cohort(
      Subject(..)
    , ID
    , Population(..)
    , ObsUnit(..)
    , Cohort(..)
    , CohortSpec
    , AttritionInfo(..)
    , specifyCohort
    , makeObsUnitFeatures
    , evalCohort
    , module Hasklepias.Cohort.Criteria
    , module Hasklepias.Cohort.Index
) where

import Prelude                  ( Eq, Show, Bool, init )
import GHC.Num                  ( Num((+)), Natural )
import Data.Aeson               ( FromJSON, ToJSON, ToJSONKey )
import Data.Foldable ( Foldable(length) )
import Data.Function            ( ($) )
import Data.Functor             ( Functor(fmap) )
import Data.Maybe               ( Maybe(..), catMaybes )
import Data.List                ( zipWith, zip, replicate )
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict as Map   ( toList, fromListWith )
import Data.Text                ( Text )
import GHC.Generics             ( Generic )
import Hasklepias.Cohort.Index
import Hasklepias.Cohort.Criteria
import FeatureCompose


type ID = Text
newtype Subject d = MkSubject (ID, d)
    deriving (Eq, Show, Generic)

instance Functor Subject where
    fmap f (MkSubject (id, x)) = MkSubject (id, f x)

instance (FromJSON d) => FromJSON (Subject d) where

newtype Population d = MkPopulation [Subject d]
    deriving (Eq, Show, Generic)

instance (FromJSON d) => FromJSON (Population d) where

instance Functor Population where
    fmap f (MkPopulation x) = MkPopulation (fmap (fmap f) x)

newtype ObsUnit d = MkObsUnit (ID, d)
    deriving (Eq, Show, Generic)

instance (ToJSON d) => ToJSON (ObsUnit d) where

newtype Cohort d = MkCohort (AttritionInfo, [ObsUnit d])
    deriving (Eq, Show, Generic)

instance (ToJSON d) => ToJSON (Cohort d) where

getPopulation :: Population d -> [Subject d]
getPopulation (MkPopulation x) = x

getSubjectData :: Subject d -> d
getSubjectData (MkSubject (_, x)) = x

makeObsUnitFeatures :: (d1 -> d0) -> Subject d1 -> ObsUnit d0
makeObsUnitFeatures f (MkSubject (id, dat)) = MkObsUnit (id, f dat)

-- makeCohort :: (d1 -> d0) -> Population d1 -> Cohort d0
-- makeCohort f (MkPopulation x) = MkCohort (fmap (makeObsUnitFeatures f) x)

data CohortSpec d1 d0 = MkCohortSpec
        { runCriteria:: d1 -> Criteria
        -- (Feature b (Index i a))
        , runFeatures:: d1 -> d0 }

specifyCohort :: (d1 -> Criteria) -> (d1 -> d0) ->  CohortSpec d1 d0
specifyCohort = MkCohortSpec

evalCriteria :: CohortSpec d1 d0 -> Population d1 -> [Subject Criteria]
evalCriteria (MkCohortSpec runCrit _) (MkPopulation pop) = fmap (fmap runCrit) pop

evalCohortStatus :: [Subject Criteria] -> [Subject CohortStatus]
evalCohortStatus = fmap (fmap checkCohortStatus)

evalSubjectCohort :: (d1 -> d0) -> Subject CohortStatus -> Subject d1 -> Maybe (ObsUnit d0)
evalSubjectCohort f (MkSubject (id, status)) subjData =
    case status of
        Included     -> Just $ makeObsUnitFeatures f subjData
        ExcludedBy _ -> Nothing

newtype AttritionInfo = MkAttritionInfo [(CohortStatus, Natural)]
    deriving (Eq, Show, Generic)

initAttritionInfo :: Criteria -> AttritionInfo
initAttritionInfo x =
    MkAttritionInfo $ zip (initStatusInfo x) (replicate (length (getCriteria x)) 0)

instance ToJSON CohortStatus where
instance ToJSON AttritionInfo where

measureAttrition :: [Subject CohortStatus] -> AttritionInfo
measureAttrition l = MkAttritionInfo $ Map.toList $
     Map.fromListWith (+) $ fmap (\x -> (getSubjectData x, 1)) l

evalUnits :: CohortSpec d1 d0 -> Population d1 -> (AttritionInfo, [ObsUnit d0])
evalUnits spec pop =
    ( measureAttrition statuses
    , catMaybes $ zipWith (evalSubjectCohort (runFeatures spec))
                    statuses
                    (getPopulation pop))
    where crits = evalCriteria spec pop
          statuses = evalCohortStatus crits

evalCohort :: CohortSpec d1 d0 -> Population d1 -> Cohort d0
evalCohort s p = MkCohort $ evalUnits s p
