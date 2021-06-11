{-# LANGUAGE LambdaCase #-}
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

import Prelude                  ( Eq, Show, Functor(..), Bool, Monoid, Semigroup )
import GHC.Num
import Data.Aeson               ( FromJSON, ToJSON, ToJSONKey )
import Data.Function
import Data.Maybe
import Data.List
import Data.Map.Strict as Map
import Data.Semigroup
import Data.Text                ( Text )
import Data.Tuple
import GHC.Generics             ( Generic)
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

data CohortSpec b d1 d0 = MkCohortSpec
        { runCriteria:: d1 -> Criteria b
        -- (Feature b (Index i a))
        , runFeatures:: d1 -> d0 }

specifyCohort :: (d1 -> Criteria b) -> (d1 -> d0) ->  CohortSpec b d1 d0
specifyCohort = MkCohortSpec

evalCriteria :: CohortSpec b d1 d0 -> Population d1 -> [Subject (Criteria b)]
evalCriteria (MkCohortSpec runCrit _) (MkPopulation pop) = fmap (fmap runCrit) pop

evalCohortStatus :: Show b => [Subject (Criteria b)] -> [Subject CohortStatus]
evalCohortStatus = fmap (fmap checkCohortStatus)

evalSubjectCohort :: (d1 -> d0) -> Subject CohortStatus -> Subject d1 -> Maybe (ObsUnit d0)
evalSubjectCohort f (MkSubject (id, status)) subjData =
    case status of
        Included     -> Just $ makeObsUnitFeatures f subjData
        ExcludedBy _ -> Nothing


newtype AttritionInfo = MkAttritionInfo [(CohortStatus, Natural)]
    deriving (Eq, Show, Generic)

-- instance ToJSONKey CohortStatus where
instance ToJSON CohortStatus where
instance ToJSON AttritionInfo where

measureAttrition :: [Subject CohortStatus] -> AttritionInfo
measureAttrition l = MkAttritionInfo $ Map.toList $
     Map.fromListWith (+) $ fmap (\x -> (getSubjectData x, 1)) l


evalUnits :: Show b => CohortSpec b d1 d0 -> Population d1 -> (AttritionInfo, [ObsUnit d0])
evalUnits spec pop =
    ( measureAttrition statuses
    , catMaybes $ zipWith (evalSubjectCohort (runFeatures spec))
                    statuses
                    (getPopulation pop))
    where crits = evalCriteria spec pop
          statuses = evalCohortStatus crits


evalCohort :: Show b => CohortSpec b d1 d0 -> Population d1 -> Cohort d0
evalCohort s p = MkCohort $ evalUnits s p
