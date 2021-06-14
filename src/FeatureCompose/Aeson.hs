{-|
Module      : Functions for encoding Feature data
Description : Defines ToJSON instances for Features.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE OverloadedStrings #-}

module FeatureCompose.Aeson(
) where

import GHC.TypeLits                 ( KnownSymbol )
import IntervalAlgebra              ( Interval, Intervallic(end, begin) )
import FeatureCompose               ( Feature(..)
                                    , MissingReason
                                    , FeatureData(..) )
import Data.Aeson                   ( object, KeyValue((.=)), ToJSON(toJSON) )

instance (ToJSON a, Ord a, Show a)=> ToJSON (Interval a) where
    toJSON x = object ["begin" .= begin x, "end" .= end x]

instance ToJSON MissingReason

instance (ToJSON d)=> ToJSON (FeatureData d) where
    toJSON  x = case getFeatureData x of
      (Left l)  -> toJSON l
      (Right r) -> toJSON r

instance (Show b, KnownSymbol n, ToJSON b, ToJSON d) => ToJSON (Feature n b d) where
    toJSON x = object [ --"name"   .= getName x
                         "name"  .= show (getName x)
                       , "attrs" .= toJSON (getAttr x)
                       , "data"  .= toJSON (getData x) ]