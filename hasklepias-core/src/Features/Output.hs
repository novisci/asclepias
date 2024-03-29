{-|
Module      : Functions for encoding Feature data
Description : Defines ToJSON instances for Features.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Features.Output
  ( ShapeOutput(..)
  , OutputShape
  ) where

import           Data.Aeson          (KeyValue ((.=)), ToJSON (toJSON), Value,
                                      object)
import           Data.Proxy          (Proxy (Proxy))
import           Data.Typeable       (Typeable, typeRep)
import           Features.Attributes (Attributes, HasAttributes (..), Purpose,
                                      Role)
import           Features.Core       (Feature, FeatureData, FeatureProblemFlag,
                                      getFData, getFeatureData)
import           GHC.Generics        (Generic)
import           GHC.TypeLits        (KnownSymbol, symbolVal)
instance ToJSON FeatureProblemFlag

instance (ToJSON d) => ToJSON (FeatureData d) where
  toJSON x = case getFeatureData x of
    (Left  l) -> toJSON l
    (Right r) -> toJSON r

instance ToJSON Role where
instance ToJSON Purpose where
instance ToJSON Attributes where

instance (Typeable d, KnownSymbol n, ToJSON d, HasAttributes n d) =>
  ToJSON (Feature n d) where
  toJSON x = object
    [ "name" .= symbolVal (Proxy @n)
    , "attrs" .= toJSON (getAttributes @n)
    , "type" .= toJSON (show $ typeRep (Proxy @d))
    , "data" .= toJSON (getFData x)
    ]

-- TODO REFACTOR what's the phantomtype for? it is fixed to Type in the only
-- place this is used, which renders it meaningless.
-- | A type used to determine the output shape of a Feature.
data OutputShape d where
  DataOnly ::(ToJSON a, Show a) => a -> OutputShape b
  NameOnly ::(ToJSON a, Show a) => a -> OutputShape b
  AttrOnly ::(ToJSON a, Show a) => a -> OutputShape b
  NameData ::(ToJSON a, Show a) => a -> OutputShape b
  NameAttr ::(ToJSON a, Show a) => a -> OutputShape b

-- | A class that provides methods for transforming some type to an 'OutputShape'.
class (ToJSON a) => ShapeOutput a where
  dataOnly :: a -> OutputShape b
  nameOnly :: a -> OutputShape b
  attrOnly :: a -> OutputShape b
  nameData :: a -> OutputShape b
  nameAttr :: a -> OutputShape b

-- | A container for name and attributes.
data NameTypeAttr = NameTypeAttr
  { getName :: String
  , getType :: String
  , getAttr :: Attributes
  }
  deriving (Generic, Show)

instance ToJSON NameTypeAttr where
  toJSON x =
    object ["name" .= getName x, "type" .= getType x, "attrs" .= getAttr x]

instance (KnownSymbol n, Show d, ToJSON d, Typeable d, HasAttributes n d) =>
  ShapeOutput (Feature n d) where
  dataOnly x = DataOnly (getFData x)
  nameOnly x = NameOnly (symbolVal (Proxy @n))
  attrOnly x = AttrOnly (getAttributes @n)
  nameData x = NameData (symbolVal (Proxy @n), getFData x)
  nameAttr x = NameAttr
    (NameTypeAttr (symbolVal (Proxy @n))
                  (show $ typeRep (Proxy @d))
                  (getAttributes @n)
    )

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
