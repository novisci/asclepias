{-|
Module      : Functions for defining attributes Feature data
Description : Defines attributes instances for Features.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE ExistentialQuantification #-}

module FeatureCompose.Featureable (
    Featureable(..)
  , packFeature
) where

import FeatureCompose                 ( HasAttributes, Feature )
import FeatureCompose.Aeson           ()
import FeatureCompose.Attributes      ()
import GHC.TypeLits                   ( KnownSymbol )
import Data.Aeson                     ( ToJSON(toJSON) )
import Data.Typeable                  ( Typeable )

{- | Existential type to hold features -}
data Featureable = forall d . (Show d, ToJSON d) => MkFeatureable d

{- | Pack a feature into a Featurable -}
packFeature ::
  (KnownSymbol n, Show d, ToJSON d, Typeable d, HasAttributes n d) =>
  Feature n d -> Featureable
packFeature = MkFeatureable

instance Show Featureable where
  show (MkFeatureable x) = show x

instance ToJSON Featureable where
  toJSON (MkFeatureable x) = toJSON x

