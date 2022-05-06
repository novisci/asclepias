{-|
Module      : Code fact
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}

module EventData.Facts.LabValue
  ( LabValue(..)
  ) where
import           Data.Aeson                     ( FromJSON )
import           Data.Eq                        ( Eq )
import           Data.Maybe                     ( Maybe )
import           Data.Text                      ( Text )
import           GHC.Float                      ( Double )
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show )

data LabValue = LabValue
  { text   :: Maybe Text
  , number :: Maybe Double
  , units  :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON LabValue where
