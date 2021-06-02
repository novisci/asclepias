{-|
Module      : Event Data Model facts 
Description : Defines the Context type and its component types, constructors, 
              and class instances
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

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

import Prelude                  ( drop, Show, Eq, Maybe )
import Control.Lens             ( makeLenses )
import GHC.Generics             ( Generic )
import Data.Text                ( Text )
import Data.Aeson               ( FromJSON(..)
                                , genericParseJSON
                                , defaultOptions
                                , fieldLabelModifier )  

newtype DemographicsFacts = 
    DemographicsFacts { _demo :: DemographicsInfo
                      } deriving( Eq, Show, Generic )

data DemographicsInfo = 
    DemographicsInfo { _field :: DemographicsField
                     , _info :: Maybe Text
                     } deriving ( Eq, Show, Generic )

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