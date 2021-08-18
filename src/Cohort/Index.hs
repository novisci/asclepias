{-|
Module      : Cohort Index 
Description : Defines the Index and related types and functions
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE Safe #-}

module Cohort.Index(
    Index(..)
  , makeIndex
) where

import GHC.Show                    ( Show )
import GHC.Generics                ( Generic )
import Data.Eq                     ( Eq )
import IntervalAlgebra             ( Intervallic(..) )
import Data.Aeson                  ( ToJSON )

{-|
An @Index@ is a wrapper for an @Intervallic@ used to indicate that a particular
interval is considered an index interval to which other intervals will be compared.
-}

newtype Index i a = MkIndex { 
    getIndex :: i a -- ^ Unwrap an @Index@
  } deriving (Eq, Show, Generic)

-- | Creates a new @'Index'@.
makeIndex :: Intervallic i a => i a -> Index i a
makeIndex = MkIndex

instance (Intervallic i a) => Intervallic (Index i) a where
  getInterval (MkIndex x) = getInterval x
  setInterval (MkIndex x) y = MkIndex (setInterval x y)

instance (Intervallic i a, ToJSON (i a)) => ToJSON (Index i a) 
