{-|
Module      : Provider fact
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}

module EventData.Context.Facts.Provider
  ( Provider(..)
  ) where
import           Data.Aeson                     ( FromJSON )
import           Data.Eq                        ( Eq )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show )

-- | provider
data Provider = Provider
  { provider_id   :: Maybe Text
  , provider_type :: Maybe Text
  , taxonomy      :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Provider where
