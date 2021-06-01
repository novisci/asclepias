{-|
Module      : Define and evaluate Features 
Description : Defines the Feature type and its component types, constructors, 
              and class instances
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module FeatureCompose(
    -- * Types
      FeatureSpec(..)
    , Feature(..)
    , FeatureData(..)
    , MissingReason(..)
    -- , FeatureDefinition(..)
    , FeatureDefinition(..)
    , makeFeatureSpec
    , featureDataR
    , featureDataL
    , define
    , defineM
    , define2
    , defineM2
    , eval
) where

import safe GHC.Read                   ( Read )
import safe GHC.Show                   ( Show(show) )
import safe GHC.Generics               ( Generic )
import safe Control.Applicative        ( Applicative(..) )
import safe Control.Monad              ( Functor(..), Monad(..), join, liftM, liftM2)
import safe Data.Either                ( Either(..) )
import safe Data.Eq                    ( Eq )
import safe Data.Function              ( ($), (.) )
import safe Data.List                  ( (++), zipWith )
import safe Data.Maybe                 ( Maybe(..), maybe )
import safe Data.Ord                   ( Ord )
import safe Data.Traversable           ( Traversable(..) )
import safe Data.Text                  ( Text )
import safe Data.Tuple                 ( uncurry, curry )

-- import safe Test.QuickCheck       ( Property )

{- | A 'FeatureSpec' contains all the information needed to derive a 'Feature':
      * its name
      * its attributes
      * the function needed to derive a feature (i.e. the 'FeatureDefinition')
-}
data (Show b) => FeatureSpec b di d0 = MkFeatureSpec {
        getSpecName :: Text
      , getSpecAttr :: b
      , getDefn :: FeatureDefinition di d0
      -- To add in future: an optional list of properties to check
      -- , getProp :: Maybe [Feature d -> Events a -> Property] 
    }

-- | TODO
makeFeatureSpec :: Show b =>
     Text
  -> b
  -> FeatureDefinition di d0
  -> FeatureSpec b di d0
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

instance (Show b, Show d) => Show (Feature b d) where
    show x = "(" ++ show (getName x) ++ ": (" ++ show (getAttr x) ++ ") "  ++ show (getData x) ++ " )\n"

instance (Show b) => Functor (Feature b) where
  fmap f (MkFeature n a d) = MkFeature n a (fmap f d)

{- | 'FeatureData' is @'Either' 'MissingReason' d@, where @d@ can be any type 
     of data derivable from 'Hasklepias.Event.Events'.
-}
newtype FeatureData d = MkFeatureData { getFeatureData :: Either MissingReason [d] }
  deriving (Generic, Show, Eq)

instance Functor FeatureData where
  fmap f (MkFeatureData x) = MkFeatureData (fmap (fmap f) x)

instance Applicative FeatureData where
  pure = featureDataR . pure
  liftA2 f (MkFeatureData x) (MkFeatureData y) =
    MkFeatureData ( liftA2 (zipWith f) x y )

instance Monad FeatureData where
  (MkFeatureData x) >>= f = -- TODO: surely there's a cleaner way
    case fmap (fmap f) x of
         Left l  -> featureDataL l
         Right v -> case getFeatureData (sequenceA v) of
                      Left l  -> featureDataL l
                      Right v -> MkFeatureData $ Right (join v)

-- | Create the 'Right' side of 'FeatureData'.
featureDataR :: [d] -> FeatureData d
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

-- TODO: the code below should be generalized so that there is a single define/eval
--       interface and the recursive structure is realizing and not hacked together.
newtype FeatureDefinition di d0 = MkFeatureDefinition (di -> FeatureData d0)

class Eval di d0 where
  eval :: FeatureDefinition di d0 -> di -> FeatureData d0
  eval (MkFeatureDefinition f) = f

instance Eval (FeatureData d1) d0 where
instance Eval (FeatureData d2, FeatureData d1) d0 where

defineM :: (d1 -> FeatureData d0) -> FeatureDefinition (FeatureData d1) d0
defineM f = MkFeatureDefinition (>>= f)

defineM2 :: (d2 -> d1 -> FeatureData d0) -> FeatureDefinition (FeatureData d2, FeatureData d1) d0
defineM2 f = MkFeatureDefinition (\ (x, y) -> join (liftA2 f x y))

define :: (d1 -> d0) -> FeatureDefinition (FeatureData d1) d0
define f = MkFeatureDefinition (fmap f)

define2 :: (d2 -> d1 -> d0) -> FeatureDefinition (FeatureData d2, FeatureData d1) d0
define2 f = MkFeatureDefinition $ uncurry (liftA2 f)
