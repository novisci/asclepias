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
TODO: describe me.
-}

newtype (Intervallic i a) => Index i a = MkIndex (i a)
  deriving (Eq, Show)

makeIndex :: Intervallic i a => i a -> Index i a
makeIndex = MkIndex

getIndex :: Intervallic i a => Index i a  -> i a 
getIndex (MkIndex x) = x

