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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module FeatureCompose.FeatureOutput(
    ShapeOutput(..)
  , OutputShape
) where

import GHC.Generics                 ( Generic )
import GHC.TypeLits                 ( KnownSymbol, symbolVal )
import IntervalAlgebra              ( Interval, Intervallic(end, begin) )
import FeatureCompose               ( Feature
                                    , MissingReason
                                    , FeatureData
                                    , getFeatureData
                                    , getFData
                                    , HasAttributes(..) )
import FeatureCompose.Attributes    ( Attributes, Purpose, Role )
import Data.Aeson                   ( object
                                    , KeyValue((.=))
                                    , ToJSON(toJSON)
                                    , Value )
import Data.Proxy                   ( Proxy(Proxy) )
import Data.Typeable                ( typeRep, Typeable )

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

instance (Typeable d, KnownSymbol n, ToJSON d, HasAttributes n d) =>
  ToJSON (Feature n d) where
    toJSON x = object [  "name"  .= symbolVal (Proxy @n)
                       , "attrs" .= toJSON (getAttributes x)
                       , "type"  .= toJSON (show $ typeRep (Proxy @d))
                       , "data"  .= toJSON (getFData x) ]

-- | A type used to determine the output shape of a Feature.
data OutputShape d where
  DataOnly :: (ToJSON a, Show a) => a -> OutputShape b
  NameOnly :: (ToJSON a, Show a) => a -> OutputShape b
  AttrOnly :: (ToJSON a, Show a) => a -> OutputShape b
  NameData :: (ToJSON a, Show a) => a -> OutputShape b
  NameAttr :: (ToJSON a, Show a) => a -> OutputShape b 

class (ToJSON a) => ShapeOutput a where
  dataOnly :: a -> OutputShape b
  nameOnly :: a -> OutputShape b
  attrOnly :: a -> OutputShape b
  nameData :: a -> OutputShape b
  nameAttr :: a -> OutputShape b

-- | A container for name and attributes.
data NameTypeAttr = NameTypeAttr { 
      getName :: String
    , getType :: String
    , getAttr :: Attributes }
  deriving (Generic, Show)

instance ToJSON NameTypeAttr where
  toJSON x = object [ "name"  .= getName x
                    , "type"  .= getType x
                    , "attrs" .= getAttr x]  

instance (KnownSymbol n, Show d, ToJSON d, Typeable d, HasAttributes n d) => 
  ShapeOutput (Feature n d) where
  dataOnly x = DataOnly (getFData x)
  nameOnly x = NameOnly (symbolVal (Proxy @n))
  attrOnly x = AttrOnly (getAttributes x)
  nameData x = NameData (symbolVal (Proxy @n), getFData x)
  nameAttr x = NameAttr (NameTypeAttr (symbolVal (Proxy @n)) (show $ typeRep (Proxy @d)) (getAttributes x))

instance ToJSON (OutputShape a) where
  toJSON (DataOnly x) = toJSON x
  toJSON (NameOnly x) = toJSON x
  toJSON (AttrOnly x) = toJSON x
  toJSON (NameData x) = toJSON x
  toJSON (NameAttr x) = toJSON x

instance Show (OutputShape a) where
  show (DataOnly x) = show x
  show (NameOnly x) = show x
  show (AttrOnly x) = show x
  show (NameData x) = show x
  show (NameAttr x) = show x