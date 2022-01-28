{-|
Module      : Exchange fact
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}

module EventData.Context.Facts.Exchange
  ( Exchange(..)
  ) where
import           Data.Aeson                     ( FromJSON )
import           Data.Eq                        ( Eq )
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show )

data Exchange =
      UnknownExchange -- TODO: Different from EDM to avoid conflicting exports; deal with this later
    | None
    | Group
    | IndFederal
    | IndState
    | Medicaid
    | Medicare
    | ThirdParty
   deriving (Eq, Show, Generic)

instance FromJSON Exchange where
