{-|
Module      : Cohort Index 
Description : Defines the Index and related types and functions
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe #-}

module Hasklepias.Cohort.Index(
    Index
  , makeIndex
  , getIndex
) where

import safe GHC.Show                    ( Show )
import safe Data.Eq                     ( Eq )
import safe IntervalAlgebra             ( Intervallic )

{-|
An @Index@ is a wrapper for an @Intervallic@ used to indicate that a particular
interval is considered an index interval to which other intervals will be compared.
-}

newtype (Intervallic i a) => Index i a = MkIndex { 
    getIndex :: i a -- ^ Unwrap an @Index@
  } deriving (Eq, Show)

-- | Creates a new @'Index'@.
makeIndex :: Intervallic i a => i a -> Index i a
makeIndex = MkIndex


