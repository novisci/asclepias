{-|
Module      : Hospitalization fact
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module EventData.Context.Facts.Hospitalization
  ( Hospitalization(..)
  ) where
import           Data.Aeson                     ( FromJSON )
import           Data.Eq                        ( Eq )
import           Data.Maybe                     ( Maybe )
import           Data.Text                      ( Text )
import           GHC.Float                      ( Double )
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show )

data Hospitalization = Hospitalization
  { discharge_status :: Maybe Text
  , length_of_stay   :: Maybe Double
  }
  deriving (Eq, Show, Generic)

instance FromJSON Hospitalization where
