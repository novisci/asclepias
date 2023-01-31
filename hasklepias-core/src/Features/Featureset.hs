{-|
Module      : Featureset
Description : Defines a collection of Featureables.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Features.Featureset
  ( Featureset
  , FeaturesetList(..)
  , featureset
  , getFeatureset
  , getFeaturesetAttrs
  , getFeaturesetList
  , tpose
  , allEqFeatureableData
  ) where

import           Data.Aeson           (ToJSON (toJSON), object, (.=))
import           Data.Function        ((.))
import           Data.Functor         (Functor (fmap))
import           Data.List.NonEmpty   (NonEmpty (..), head, transpose)
import qualified Data.List.NonEmpty   as NE
import           Features.Attributes  (Attributes)
import           Features.Featureable (Featureable, getFeatureableAttrs)
import           Features.Output      (ShapeOutput (dataOnly))
import           GHC.Generics         (Generic)
import           GHC.Show             (Show)
-- TODO REFACTOR why no prelude?
import           Prelude              (($), (==))
import qualified Prelude

-- TODO REFACTOR this is not a set
-- | A Featureset is a (non-empty) list of @Featureable@.
newtype Featureset
  = MkFeatureset (NE.NonEmpty Featureable)
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
newtype FeaturesetList
  = MkFeaturesetList (NE.NonEmpty Featureset)
  deriving (Show)

-- | Constructor of a @Featureset@.
getFeaturesetList :: FeaturesetList -> NE.NonEmpty Featureset
getFeaturesetList (MkFeaturesetList x) = x

-- | Transpose a FeaturesetList
tpose :: FeaturesetList -> FeaturesetList
tpose (MkFeaturesetList x) =
  MkFeaturesetList (fmap featureset (transpose (fmap getFeatureset x)))


  {- Utilities -}

-- TODO revisit

-- | Compare two Featuresets via their ShapeOutput and ToJSON implementations.
-- They cannot be compared directly because an existential type cannot be Eq.
-- Comparing by JSON at the moment makes the most sense because that is the
-- output format and thus how downstream applications will understand
-- "equality". Still, this should be revisited.
allEqFeatureableData :: Featureset -> Featureset -> Prelude.Bool
allEqFeatureableData (MkFeatureset f1) (MkFeatureset f2) = s1 == s2
  where s1 = NE.sort $ NE.map (toJSON . dataOnly) f1
        s2 = NE.sort $ NE.map (toJSON . dataOnly) f2
