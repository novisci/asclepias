{-|
Module      : Claim fact
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}

module EventData.Context.Facts.Claim
  ( Claim(..)
  ) where
import           Data.Aeson                     ( FromJSON )
import           Data.Eq                        ( Eq )
import           Data.Maybe                     ( Maybe )
import           Data.Text                      ( Text )
import           EventData.Context.Facts.Codebook
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( Integer )
import           GHC.Show                       ( Show )

data Claim = Claim
  { id        :: Text
  , _type     :: Maybe Text -- TODO: field does not actually have underscore
                        -- ; using it to avoid clash with "type" keyword 
  , index     :: Maybe Integer
  , procedure :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Claim where
