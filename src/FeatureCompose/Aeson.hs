{-|
Module      : Functions for encoding Feature data
Description : Defines ToJSON instances for Features.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FeatureCompose.Aeson(
) where

import GHC.TypeLits                 ( KnownSymbol, symbolVal )
import IntervalAlgebra              ( Interval, Intervallic(end, begin) )
import FeatureCompose               ( Feature
                                    , MissingReason
                                    , FeatureData
                                    , getFeatureData
                                    , getData )
import Data.Aeson                   ( object, KeyValue((.=)), ToJSON(toJSON) )
import Data.Proxy                   ( Proxy(Proxy) )

instance (ToJSON a, Ord a, Show a)=> ToJSON (Interval a) where
    toJSON x = object ["begin" .= begin x, "end" .= end x]

instance ToJSON MissingReason

instance (ToJSON d)=> ToJSON (FeatureData d) where
    toJSON  x = case getFeatureData x of
      (Left l)  -> toJSON l
      (Right r) -> toJSON r

instance (KnownSymbol n, ToJSON d) => ToJSON (Feature n d) where
    toJSON x = object [ --"name"   .= getName x
                         "name"  .= show (symbolVal (Proxy @n))
                      --  , "attrs" .= toJSON (getAttr x)
                       , "data"  .= toJSON (getData x) ]