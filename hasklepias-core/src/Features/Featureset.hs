{-|
Module      : Featureset
Description : Defines a collection of Featureables.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Features.Featureset
  ( Featureset
  , FeaturesetList(..)
  , featureset
  , getFeatureset
  , getFeaturesetAttrs
  , getFeaturesetList
  , tpose
  ) where

import           Data.Aeson                     ( (.=)
                                                , ToJSON(toJSON)
                                                , object
                                                )
import           Data.Function                  ( (.) )
import           Data.Functor                   ( Functor(fmap) )
import           Data.List.NonEmpty            as NE
                                                ( NonEmpty(..)
                                                , head
                                                , transpose
                                                )
import           Features.Attributes            ( Attributes )
import           Features.Featureable           ( Featureable
                                                , getFeatureableAttrs
                                                )
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show )

-- | A Featureset is a (non-empty) list of @Featureable@.
newtype Featureset = MkFeatureset (NE.NonEmpty Featureable)
  deriving (Show)

-- | Constructor of a @Featureset@.
featureset :: NE.NonEmpty Featureable -> Featureset
featureset = MkFeatureset

-- | Constructor of a @Featureset@.
getFeatureset :: Featureset -> NE.NonEmpty Featureable
getFeatureset (MkFeatureset x) = x

-- | Gets a list of @Attributes@ from a @Featureset@, one @Attributes@ per @Featureable@.
getFeaturesetAttrs :: Featureset -> NE.NonEmpty Attributes
getFeaturesetAttrs (MkFeatureset l) = fmap getFeatureableAttrs l

instance ToJSON Featureset where
  toJSON (MkFeatureset x) = toJSON x

-- | A newtype wrapper for a 'NE.NonEmpty' 'Featureset'.
newtype FeaturesetList = MkFeaturesetList (NE.NonEmpty Featureset)
  deriving (Show)

-- | Constructor of a @Featureset@.
getFeaturesetList :: FeaturesetList -> NE.NonEmpty Featureset
getFeaturesetList (MkFeaturesetList x) = x

-- | Transpose a FeaturesetList
tpose :: FeaturesetList -> FeaturesetList
tpose (MkFeaturesetList x) =
  MkFeaturesetList (fmap featureset (transpose (fmap getFeatureset x)))
