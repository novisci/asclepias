{-|
Module      : Event Data Diagnosis Domain
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}

module EventData.Context.Domain.Diagnosis(
  DiagnosisFacts(..)
) where

import Data.Aeson               ( FromJSON )
import Data.Eq                  ( Eq )
import Data.Maybe               ( Maybe )
import GHC.Generics             ( Generic )
import GHC.Show                 ( Show )
import EventData.Context.Facts

-- | An enrollment fact
data DiagnosisFacts = DiagnosisFacts {
       code :: Code
     , claim :: Maybe Claim
     , location :: Maybe Location
  } 
  deriving( Eq, Show, Generic )


instance FromJSON DiagnosisFacts where
