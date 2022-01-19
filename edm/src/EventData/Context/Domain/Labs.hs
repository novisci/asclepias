{-|
Module      : Event Data Labs Domain
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module EventData.Context.Domain.Labs
  ( LabsFacts(..)
  ) where

import           Data.Aeson                     ( FromJSON )
import           Data.Eq                        ( Eq )
import           Data.Maybe                     ( Maybe )
import           EventData.Context.Facts
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show )

-- | An enrollment fact
data LabsFacts = LabsFacts
  { code     :: Code
  , claim    :: Maybe Claim
  , location :: Maybe Location
  , value    :: LabValue
  }
  deriving (Eq, Show, Generic)


instance FromJSON LabsFacts where
