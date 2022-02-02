{-|
Module      : Cohort Index 
Description : Defines the Index and related types and functions
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

module Cohort.Index
  (
  {- |
    An 'Index' is an interval of time from which the assessment intervals for an
   observational unit may be derived. Assessment intervals (encoded in the type
   'Cohort.AssessmentInterval') are intervals of time during which features are evaluated. 
   -}
    Index
  , makeIndex
  , getIndex
  , IndexSet(..)
  , makeIndexSet
  , FIndex
  ) where

import           Data.Aeson                     ( ToJSON )
import           Data.Bool                      ( Bool(..) )
import           Data.Eq                        ( Eq )
import           Data.Functor                   ( Functor(fmap) )
import           Data.List                      ( null )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Ord                       ( Ord(..) )
import qualified Data.Set                      as Set
                                                ( Set
                                                , fromList
                                                )
import           Features
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show )
import           IntervalAlgebra                ( Interval
                                                , Intervallic(..)
                                                )
{-|
An @Index@ is a wrapper for an @Intervallic@ used to indicate that a particular
interval is considered an index interval to which other intervals will be compared.
-}
newtype Index i a = MkIndex { 
    getIndex :: i a -- ^ unwrap an `Index` 
  }
  deriving (Eq, Show, Generic)

-- | Creates a new @'Index'@.
makeIndex :: Intervallic i a => i a -> Index i a
makeIndex = MkIndex

instance (Functor i) => Functor (Index i) where
  fmap f (MkIndex x) = MkIndex (fmap f x)

instance (Intervallic i a) => Intervallic (Index i) a where
  getInterval (MkIndex x) = getInterval x
  setInterval (MkIndex x) y = MkIndex (setInterval x y)

instance (Ord (i a)) => Ord (Index i a) where
  compare (MkIndex x) (MkIndex y) = compare x y

instance (Intervallic i a, ToJSON (i a)) => ToJSON (Index i a)

-- | A type containing (maybe) a @Data.Set.Set@ of indices.
newtype IndexSet i a = MkIndexSet ( Maybe (Set.Set (Index i a)))
  deriving (Eq, Show, Generic)

{-|
Utility for creating an `IndexSet` from a list of `Index`.
-}
makeIndexSet :: (Ord (i a), Intervallic i a) => [Index i a] -> IndexSet i a
makeIndexSet x =
  if null x then MkIndexSet Nothing else MkIndexSet (Just (Set.fromList x))

-- | Simply a @'Feature'@ of an @'Index'@ named @"index"@.
type FIndex n i a = Feature n (Index i a)
