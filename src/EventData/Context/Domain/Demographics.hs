{-|
Module      : Event Data Model facts 
Description : Defines the Context type and its component types, constructors, 
              and class instances
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module EventData.Context.Domain.Demographics(
      DemographicsFacts(..)
    , DemographicsInfo(..)
    , DemographicsField(..)
    , demo 
    , field 
    , info
) where

import Control.Lens             ( makeLenses )

import Data.Aeson               ( FromJSON(..)
                                , genericParseJSON
                                , defaultOptions
                                , fieldLabelModifier )
import Data.List                ( drop )
import Data.Eq                  ( Eq )
import Data.Maybe               ( Maybe )
import Data.Text                ( Text )
import GHC.Generics             ( Generic )
import GHC.Show                 ( Show )


-- | a demographic fact
newtype DemographicsFacts = 
    DemographicsFacts { _demo :: DemographicsInfo
                      } deriving( Eq, Show, Generic )

-- | information of a demographic fact
data DemographicsInfo = 
    DemographicsInfo { _field :: DemographicsField
                     , _info :: Maybe Text
                     } deriving ( Eq, Show, Generic )

-- | fields available in a demographic fact
data DemographicsField =
      BirthYear
    | BirthDate
    | Race
    | RaceCodes
    | Gender
    | Zipcode
    | County
    | CountyFIPS
    | State
    | Ethnicity
    | Region
    | UrbanRural
    | GeoPctAmIndian
    | GeoPctAsian
    | GeoPctBlack
    | GeoPctHispanic
    | GeoPctMutli
    | GeoPctOther
    | GeoPctWhite
    | GeoType
    | GeoAdiStateRank
    | GeoAdiNatRank
    deriving ( Eq, Show, Generic )

makeLenses ''DemographicsFacts
makeLenses ''DemographicsInfo

instance FromJSON DemographicsFacts where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}
instance FromJSON DemographicsInfo where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = drop 1}
instance FromJSON DemographicsField where