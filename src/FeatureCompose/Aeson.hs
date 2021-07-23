{-|
Module      : Functions for encoding Feature data
Description : Defines ToJSON instances for Features.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module FeatureCompose.Aeson(
) where

import GHC.TypeLits                 ( KnownSymbol, symbolVal )
import IntervalAlgebra              ( Interval, Intervallic(end, begin) )
import FeatureCompose               ( Feature
                                    , MissingReason
                                    , FeatureData
                                    , getFeatureData
                                    , getFData
                                    , HasAttributes(..) )
import FeatureCompose.Attributes
import Data.Aeson                   ( object, KeyValue((.=)), ToJSON(toJSON) )
import Data.Proxy                   ( Proxy(Proxy) )
import Data.Typeable (typeRep, Typeable)

instance (ToJSON a, Ord a, Show a)=> ToJSON (Interval a) where
    toJSON x = object ["begin" .= begin x, "end" .= end x]

instance ToJSON MissingReason

instance (ToJSON d) => ToJSON (FeatureData d) where
    toJSON  x = case getFeatureData x of
      (Left l)  -> toJSON l
      (Right r) -> toJSON r

instance ToJSON Role where
instance ToJSON Purpose where
instance ToJSON Attributes where

instance (Typeable d, KnownSymbol n, ToJSON d, HasAttributes n d) => ToJSON (Feature n d) where
    toJSON x = object [  "name"  .= symbolVal (Proxy @n)
                       , "attrs" .= toJSON (getAttributes x)
                       , "type"  .= toJSON (show $ typeRep (Proxy @d))
                       , "data"  .= toJSON (getFData x) ]