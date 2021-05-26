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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Hasklepias.Types.Feature(
    -- * Types
      FeatureSpec(..)
    , Feature(..)
    , FeatureData(..)
    , MissingReason(..)
    , FeatureDefinition(..)
    , makeFeatureSpec
    , featureDataR
    , featureDataL
    , define0
    , define1
    , define2
    , define2d
    , eval0
    , eval1
    , eval2
    , evalSpec0
    , evalSpec1
    , evalSpec2
) where

import safe GHC.Read                   ( Read )
import safe GHC.Show                   ( Show(show) )
import safe GHC.Generics               ( Generic, D )
import safe Control.Applicative        ( Applicative(..) )
import safe Control.Monad              ( Functor(..), Monad(..), join, liftM2)
import safe Data.Either                ( Either(..) )
import safe Data.Eq                    ( Eq )
import safe Data.Function              ( ($), (.) )
import safe Data.List             ( (++) )  
import safe Data.Maybe                 ( Maybe(..), maybe )
import safe Data.Ord                   ( Ord )
import safe Data.Text                  ( Text )
-- import safe Test.QuickCheck       ( Property )

{- | A 'FeatureSpec' contains all the information needed to derive a 'Feature':
      * its name
      * its attributes
      * the function needed to derive a feature (i.e. the 'FeatureDefinition')
-}
data (Show b) => FeatureSpec b f e d = MkFeatureSpec {
        getSpecName :: Text
      , getSpecAttr :: b
      , getDefn :: FeatureDefinition f e d
      -- To add in future: an optional list of properties to check
      -- , getProp :: Maybe [Feature d -> Events a -> Property] 
    }

-- | TODO
makeFeatureSpec :: Show b =>
     Text
  -> b
  -> FeatureDefinition f e d
  -> FeatureSpec b f e d
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

instance (Show b, Show d) => Show (Feature d b) where 
    show x = "(" ++ show (getName x) ++ ": (" ++ show (getAttr x) ++ ") "  ++ show (getData x) ++ " )\n"

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


-- TODO: the code below should be generalized so that there is a single define/eval
--       interface.
-- | A type to hold FeatureData definitions; i.e. functions that return 
--  features. 
data FeatureDefinition f e d =
    FD0 (e -> FeatureData d)
  | FD1 (FeatureData e -> FeatureData d)
  | FD2 (FeatureData f -> FeatureData e -> FeatureData d)

define0 :: (e -> FeatureData d) -> FeatureDefinition * e d
define0 = FD0

eval0 :: FeatureDefinition * e d -> e -> FeatureData d
eval0 (FD0 f) = f

evalSpec0 :: (Show b) => FeatureSpec b * e d -> e -> Feature b d
evalSpec0 (MkFeatureSpec nm attr def) y = MkFeature nm attr (eval0 def y)

define1 :: (e -> d) -> FeatureDefinition * e d
define1 f = FD1 $ fmap f

eval1 :: FeatureDefinition * e d -> FeatureData e -> FeatureData d
eval1 (FD1 f) = f

evalSpec1 :: (Show b) => FeatureSpec b * e d -> Feature b e -> Feature b d
evalSpec1 (MkFeatureSpec nm attr def) y = MkFeature nm attr (eval1 def (getData y))

define2 :: (f -> e -> d) -> FeatureDefinition f e d
define2 f =  FD2 $ liftA2 f

define2d :: (f -> e -> FeatureData d) -> FeatureDefinition f e d 
define2d f = FD2 (\x y -> join (liftM2 f x y))

eval2 :: FeatureDefinition f e d -> FeatureData f -> FeatureData e -> FeatureData d
eval2 (FD2 f) = f 

evalSpec2 :: (Show b) => FeatureSpec b f e d -> Feature b f -> Feature b e -> Feature b d
evalSpec2 (MkFeatureSpec nm attr def) y z = MkFeature nm attr (eval2 def (getData y) (getData z))
