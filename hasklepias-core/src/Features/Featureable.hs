{-|
Module      : Featureable
Description : Defines an existential type to with which to collect Features.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

module Features.Featureable
  ( Featureable(..)
  , packFeature
  , getFeatureableAttrs
  ) where

import           Data.Aeson          (ToJSON (toJSON))
import           Data.Typeable       (Typeable)
import           Features.Attributes (Attributes, HasAttributes (..))
import           Features.Core       (Feature, makeFeature)
import           Features.Output     (OutputShape, ShapeOutput (..))
import           GHC.TypeLits        (KnownSymbol)

{- | Existential type to hold features, which allows for Features to be put
into a homogeneous list.
-}
data Featureable
  = forall d. (Show d, ToJSON d, ShapeOutput d) => MkFeatureable d Attributes

{- | Pack a feature into a @Featurable@. -}
packFeature
  :: forall n d
   . (KnownSymbol n, Show d, ToJSON d, Typeable d, HasAttributes n d)
  => Feature n d
  -> Featureable
packFeature x = MkFeatureable x (getAttributes @n)

-- TODO REFACTOR why are attributes there if not in show or
-- json?
instance Show Featureable where
  show (MkFeatureable x _) = show x

instance ToJSON Featureable where
  toJSON (MkFeatureable x _) = toJSON x

instance ShapeOutput Featureable where
  dataOnly (MkFeatureable x _) = dataOnly x
  nameOnly (MkFeatureable x _) = nameOnly x
  attrOnly (MkFeatureable x _) = attrOnly x
  nameData (MkFeatureable x _) = nameData x
  nameAttr (MkFeatureable x _) = nameAttr x

-- | Get the @Attributes@ from a @Featureable@.
getFeatureableAttrs :: Featureable -> Attributes
getFeatureableAttrs (MkFeatureable _ a) = a


  {- Utilities -}

-- | Compare two featurables via their ShapeOutput and ToJSON implementations.
-- See @Features.Featureset.'allEqFeatureable'@.
eqFeatureableData :: Featureable -> Featureable -> Bool
eqFeatureableData f1 f2 = toJSON (dataOnly f1) == toJSON (dataOnly f2)
