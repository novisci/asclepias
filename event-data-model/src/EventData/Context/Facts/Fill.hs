{-|
Module      : Code fact
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}

module EventData.Context.Facts.Fill
  ( Fill(..)
  ) where
import           Data.Aeson                     ( FromJSON )
import           Data.Eq                        ( Eq )
import           Data.Maybe                     ( Maybe )
import           Data.Text                      ( Text )
import           GHC.Float                      ( Double )
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( Integer )
import           GHC.Show                       ( Show )

data Fill = Fill
  { days_supply :: Maybe Integer
  , quantity    :: Maybe Double
  , strength    :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Fill where
