{-|
Module      : Featureset
Description : Defines a collection of Featureables.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE NoImplicitPrelude #-}

module FeatureCompose.Featureset (
    Featureset 
  , featureset
  , getFeatureset
  , getFeaturesetAttrs
) where

import FeatureCompose.Featureable    ( Featureable
                                      , getFeatureableAttrs
                                      )
import FeatureCompose.Attributes      ( Attributes )

import Data.Aeson                     ( ToJSON(toJSON) )
import Data.Functor                   ( Functor(fmap) )
import GHC.Show ( Show )


-- | A Featureset is a list of @Featureable@.
newtype Featureset = MkFeatureset [Featureable]
  deriving (Show)

-- | Constructor of a @Featureset@.
featureset :: [Featureable] -> Featureset
featureset = MkFeatureset

-- | Constructor of a @Featureset@.
getFeatureset :: Featureset -> [Featureable]
getFeatureset (MkFeatureset x) = x

-- | Gets a list of @Attributes@ from a @Featureset@, one @Attributes@ per @Featureable@.
getFeaturesetAttrs :: Featureset -> [Attributes]
getFeaturesetAttrs (MkFeatureset l) = fmap getFeatureableAttrs l

instance ToJSON Featureset where
  toJSON (MkFeatureset x) = toJSON x
