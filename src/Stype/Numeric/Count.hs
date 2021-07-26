{-|
Module      : Stype count
Description : Statistical types
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}

{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric #-}

module Stype.Numeric.Count (
  Count(..)
) where

import safe GHC.Generics                 ( Generic )
import safe GHC.Num                      ( Natural )
import safe Data.Semiring                ( Semiring(..) )

newtype Count = Count Natural
  deriving (Eq, Show, Ord, Generic)

instance Num Count where
  (+) (Count x) (Count y) = Count (x + y)
  (*) (Count x) (Count y) = Count (x * y) 
  fromInteger x = Count $ fromInteger x
  abs = id -- useless
  signum x = Count 1 -- useless
  negate = id -- useless

instance Semigroup Natural where
  (<>) x y = x + y

instance Monoid Natural where
  mempty = 0

instance Semiring Natural where
  one = 1
  (<.>) = (*)

instance Semigroup Count where
  (<>) (Count x) (Count y) = Count (x <> y)

instance Monoid Count where
  mempty = Count 0

instance Semiring Count where
  one = Count 1
  (<.>) = (*)

