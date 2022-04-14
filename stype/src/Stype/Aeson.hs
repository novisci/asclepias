{-|
Module      : Stype aeson instances
Description : Statistical types
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stype.Aeson
  () where

import           Data.Aeson
import           Data.Text
import           Stype.Categorical              ( Binary
                                                , Nominal
                                                , toBool
                                                )
-- ( ToJSON(..)) 
import           Stype.Numeric                  ( Continuous(..)
                                                , Count
                                                , EventTime(..)
                                                , MaybeCensored
                                                , NonnegContinuous(..)
                                                )

instance ToJSON Count where

instance ToJSON a => ToJSON (Continuous a) where
  toJSON (Cont x)   = toJSON x
  toJSON NegContInf = "-Inf"
  toJSON ContInf    = "Inf"

instance ToJSON a => ToJSON (NonnegContinuous a) where
  toJSON (NonNegCont x) = toJSON x
  toJSON NonNegContInf  = "Inf"

instance ToJSON a => ToJSON (EventTime a) where
  toJSON (EventTime x) = toJSON x

instance ToJSON a => ToJSON (MaybeCensored a) where

instance ToJSON a => ToJSON (Nominal a) where

instance ToJSON Binary where
  toJSON x = toJSON (toBool x)
