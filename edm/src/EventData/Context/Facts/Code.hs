{-|
Module      : Code fact
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}

module EventData.Context.Facts.Code(
  Code(..)
) where
import EventData.Context.Facts.Codebook
import Data.Aeson               ( FromJSON )
import Data.Eq                  ( Eq )
import Data.Maybe               ( Maybe )
import Data.Text                ( Text )
import GHC.Generics             ( Generic )
import GHC.Show                 ( Show )

data Code = Code {
    code :: Text
  , codebook :: Maybe Codebook
} deriving (Eq, Show, Generic)

instance FromJSON Code where