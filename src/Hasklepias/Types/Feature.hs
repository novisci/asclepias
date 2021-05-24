{-|
Module      : Hasklepias Feature Type
Description : Defines the Feature type and its component types, constructors, 
              and class instances
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}

module Hasklepias.Types.Feature(
    -- * Types
      FeatureSpec(..)
    , Feature(..)
    , FeatureData(..)
    , MissingReason(..)
    , FeatureDefinition(..)
    , Defineable(..)
    , maybeFeature
    , featureDataR
    , featureDataL
) where

import GHC.Read                   ( Read )
import GHC.Show                   ( Show )
import GHC.Generics               ( Generic )
import Data.Either                ( Either(..) )
import Data.Eq                    ( Eq )
import Data.Functor               ( Functor(fmap) )
import Data.Function              ( ($), (.) )
import Data.Maybe                 ( Maybe(..), maybe )
import Data.Ord                   ( Ord )
import Data.Text                  ( Text )
import Hasklepias.Types.Event     ( Event, Events )
import IntervalAlgebra            ( Interval, Intervallic )
-- import safe Test.QuickCheck       ( Property )

{- | A 'FeatureSpec' contains all the information needed to derive a 'Feature':
      * its name
      * its attributes
      * the function needed to derive a feature (i.e. the 'FeatureDefinition')
-}
data (Show b) => FeatureSpec b k d = FeatureSpec {
        getSpecName :: Text
      , getSpecAttr :: b
      , getDefn :: FeatureDefinition k d
      -- To add in future: an optional list of properties to check
      -- , getProp :: Maybe [Feature d -> Events a -> Property] 
    }

-- | TODO
makeFeatureSpec :: Show b => Text -> b -> FeatureDefinition k d ->
  FeatureSpec b k d
makeFeatureSpec = FeatureSpec

{- | A 'Feature' contains the following:
      * a name
      * its attributes
      * 'FeatureData'
-}
data (Show b) => Feature b d = Feature {
        getName :: Text
      , getAttr :: b
      , getData :: FeatureData d
      }

{- | 'FeatureData' is @'Either' 'MissingReason' d@, where @d@ can be any type 
     of data derivable from 'Hasklepias.Event.Events'.
-}
newtype FeatureData d = FeatureData { getFeatureData :: Either MissingReason d }
  deriving (Generic, Show, Eq)

instance Functor FeatureData where
  fmap f (FeatureData x) = FeatureData (fmap f x)

-- | Create the 'Right' side of 'FeatureData'.
featureDataR :: d -> FeatureData d
featureDataR = FeatureData . Right

-- | Create the 'Left' side of 'FeatureData'.
featureDataL :: MissingReason -> FeatureData d
featureDataL = FeatureData . Left

-- | A 'Feature' may be missing for any number of reasons. 
data MissingReason =
    InsufficientData
  | Excluded
  | Other Text
  | Unknown
  deriving (Eq, Read, Show, Generic)

-- | A type to hold FeatureData definitions; i.e. functions that return 
--  features.
newtype FeatureDefinition input d = MkFeatureDef (input -> FeatureData d)

class Defineable input where
  define :: (input -> FeatureData d) -> FeatureDefinition input d
  define = MkFeatureDef

  eval :: FeatureDefinition input d -> input -> FeatureData d
  eval (MkFeatureDef def) x = def x

instance Defineable (Events a) where
instance Defineable (FeatureData e, Events a) where
instance Defineable (FeatureData e, FeatureData f) where
instance Defineable (FeatureData e, FeatureData f, FeatureData g) where

maybeFeature :: MissingReason -> (a -> Maybe c) -> (c -> d) -> (a -> FeatureData d)
maybeFeature r f g x = maybe (featureDataL r) (featureDataR . g) (f x)