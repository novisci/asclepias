{-|
Module      : Hasklepias Subject Type
Description : Defines the Subject type
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Hasklepias.Cohort(
      Subject(..)
    , ID
    , Population(..)
    , ObsUnit(..)
    , Cohort(..)
    , makeObsUnitFeatures
    , makeCohort
) where

import Prelude                  ( Eq, Show, Functor(..) )     
import Data.Aeson               ( FromJSON, ToJSON )        
import Data.Text                ( Text )
import GHC.Generics             ( Generic)

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

newtype Cohort d = MkCohort [ObsUnit d]
    deriving (Eq, Show, Generic)

instance (ToJSON d) => ToJSON (Cohort d) where

makeObsUnitFeatures :: (d1 -> d0) -> Subject d1 -> ObsUnit d0 
makeObsUnitFeatures f (MkSubject (id, dat)) = MkObsUnit (id, f dat)

makeCohort :: (d1 -> d0) -> Population d1 -> Cohort d0
makeCohort f (MkPopulation x) = MkCohort (fmap (makeObsUnitFeatures f) x)