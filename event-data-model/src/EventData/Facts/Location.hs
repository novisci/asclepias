{-|
Module      : Location fact
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}

module EventData.Facts.Location
  ( Location(..)
  ) where
import           Data.Aeson                     ( FromJSON )
import           Data.Eq                        ( Eq )
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show )

data Location =
    Inpatient
  | Outpatient
  | Unknown
   deriving (Eq, Show, Generic)

instance FromJSON Location where
