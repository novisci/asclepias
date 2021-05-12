{-|
Module      : Hasklepias Feature Type
Description : Defines the Feature type and its component types, constructors, 
              and class instances
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Hasklepias.Types.Feature(
    -- * Types
      Feature(..)
    , MissingReason(..)

) where

import GHC.Base(String, Eq)
import GHC.Read ( Read )
import GHC.Show ( Show )
import GHC.Generics
import Data.Either ( Either )
import Data.Functor ( Functor(fmap) )

{- | A 'Feature' is a @'Either' 'MissingReason' d@, where @d@ can be any type 
     of data derivable from 'Hasklepias.Event.Events'.
-}
newtype Feature d = Feature { getFeature :: Either MissingReason d }
  deriving (Generic, Show, Eq)

instance Functor Feature where
  fmap f (Feature x) = Feature (fmap f x) 

-- | A 'Feature' may be missing for any number of reasons. 
data MissingReason =
    InsufficientData
  | Excluded
  | Other String
  | Unknown
  deriving (Eq, Read, Show, Generic)

