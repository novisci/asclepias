{-|
Module      : Code fact
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}

module EventData.Context.Facts.LabValue(
  LabValue(..)
) where
import Data.Aeson               ( FromJSON )
import Data.Eq                  ( Eq )
import Data.Maybe               ( Maybe )
import Data.Text                ( Text )
import GHC.Generics             ( Generic )
import GHC.Show                 ( Show )
import GHC.Float                ( Double )

data LabValue = LabValue {
    text :: Maybe Text
  , number :: Maybe Double
  , units :: Text
} deriving (Eq, Show, Generic)

instance FromJSON LabValue where