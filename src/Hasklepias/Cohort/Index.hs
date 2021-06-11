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
  , IndexDefinition
  , defineIndex
) where

import safe GHC.Show                    ( Show )
import safe Data.Eq                     ( Eq )
import safe Data.Function               ( ($) )
import safe Data.Functor                ( Functor(fmap) )
import safe IntervalAlgebra             ( Intervallic )
import safe FeatureCompose              ( FeatureDefinition(..)
                                        , FeatureData )

{-|
TODO: describe me.
-}

newtype (Intervallic i a) => Index i a = MkIndex (i a)
  deriving(Eq, Show)

makeIndex :: Intervallic i a => i a -> Index i a
makeIndex = MkIndex

getIndex :: Intervallic i a => Index i a  -> i a 
getIndex (MkIndex x) = x

newtype IndexDefinition di i a =
   MkIndexDefinition (FeatureDefinition di (Index i a))

-- newtype IndexFeature b i a =
   

toIndexData :: Intervallic i a => 
  FeatureDefinition di (i a) -> di -> FeatureData (Index i a)
toIndexData (MkFeatureDefinition def) x = fmap makeIndex (def x)

defineIndex :: Intervallic i a => 
  FeatureDefinition di (i a) -> IndexDefinition di i a
defineIndex def = MkIndexDefinition $ MkFeatureDefinition (toIndexData def)