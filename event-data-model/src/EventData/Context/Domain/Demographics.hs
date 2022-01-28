{-|
Module      : Event Data Demographics Domain
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module EventData.Context.Domain.Demographics
  ( DemographicsFacts(..)
  , DemographicsInfo(..)
  , DemographicsField(..)
  ) where


import           Data.Aeson                     ( FromJSON(..) )
import           Data.Eq                        ( Eq )
import           Data.Maybe                     ( Maybe )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show )


-- | a demographic fact
newtype DemographicsFacts =
    DemographicsFacts { demo :: DemographicsInfo
                      } deriving( Eq, Show, Generic )

-- | information of a demographic fact
data DemographicsInfo = DemographicsInfo
  { field :: DemographicsField
  , info  :: Maybe Text
  }
  deriving (Eq, Show, Generic)

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

instance FromJSON DemographicsFacts where
instance FromJSON DemographicsInfo where
instance FromJSON DemographicsField where
