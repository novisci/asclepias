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
{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}

module Hasklepias.Types.Feature(
    -- * Types
      FeatureSpec(..)
    , Feature(..)
    , FeatureData(..)
    , MissingReason(..)
    , FeatureDefinition(..)
    , featureDataR
    , featureDataL
) where

import GHC.Read                   ( Read )
import GHC.Show                   ( Show )
import GHC.Generics               ( Generic, D )
import Control.Applicative        ( Applicative(..) )
import Control.Monad              ( Functor(..), Monad(..))
import Data.Either                ( Either(..) )
import Data.Eq                    ( Eq )
import Data.Function              ( ($), (.) )
import Data.Maybe                 ( Maybe(..), maybe )
import Data.Ord                   ( Ord )
import Data.Text                  ( Text )
-- import safe Test.QuickCheck       ( Property )

{- | A 'FeatureSpec' contains all the information needed to derive a 'Feature':
      * its name
      * its attributes
      * the function needed to derive a feature (i.e. the 'FeatureDefinition')
-}
data (Show b) => FeatureSpec b k d = MkFeatureSpec {
        getSpecName :: Text
      , getSpecAttr :: b
      , getDefn :: FeatureDefinition k d
      -- To add in future: an optional list of properties to check
      -- , getProp :: Maybe [Feature d -> Events a -> Property] 
    } 

-- | TODO
makeFeatureSpec :: Show b => Text -> b -> FeatureDefinition k d ->
  FeatureSpec b k d
makeFeatureSpec = MkFeatureSpec

{- | A 'Feature' contains the following:
      * a name
      * its attributes
      * 'FeatureData'
-}
data (Show b) => Feature b d = MkFeature {
        getName :: Text
      , getAttr :: b
      , getData :: FeatureData d
      } deriving (Eq)

instance (Show b) => Functor (Feature b) where
  fmap f (MkFeature n a d) = MkFeature n a (fmap f d)

{- | 'FeatureData' is @'Either' 'MissingReason' d@, where @d@ can be any type 
     of data derivable from 'Hasklepias.Event.Events'.
-}
newtype FeatureData d = MkFeatureData { getFeatureData :: Either MissingReason d }
  deriving (Generic, Show, Eq)

instance Functor FeatureData where
  fmap f (MkFeatureData x) = MkFeatureData (fmap f x)

instance Applicative FeatureData where
  pure = featureDataR
  liftA2 f (MkFeatureData x) (MkFeatureData y) = MkFeatureData ( liftA2 f x y )

instance Monad FeatureData where
  return = MkFeatureData . return
  x >>= f = do x >>= f

-- | Create the 'Right' side of 'FeatureData'.
featureDataR :: d -> FeatureData d
featureDataR = MkFeatureData . Right

-- | Create the 'Left' side of 'FeatureData'.
featureDataL :: MissingReason -> FeatureData d
featureDataL = MkFeatureData . Left

-- | 'FeatureData' may be missing for any number of reasons. 
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

instance Defineable (FeatureData e) where
instance Defineable (FeatureData e, FeatureData f) where
instance Defineable (FeatureData e, FeatureData f, FeatureData g) where
