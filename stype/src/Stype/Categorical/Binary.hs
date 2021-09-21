{-|
Module      : Stype binary 
Description : Statistical types
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}

{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric #-}

module Stype.Categorical.Binary (
    Binary(..)
  , toBool
  , toInt
  , fromBool
) where

import GHC.Generics             ( Generic )

{- | Binary Type -}
data Binary = Zero | One  deriving ( Eq, Ord, Generic )

instance Show Binary where 
  show x = 
      case x of
      Zero -> "0"
      One  ->  "1"

-- | Convert a @Binary@ to @Bool@.
toBool :: Binary -> Bool 
toBool Zero = False 
toBool One  = True 

-- | Convert @Binary@ to @Int@.
toInt :: Binary -> Int 
toInt Zero = 0
toInt One  = 1

-- | Create a @Binary@ from a @Bool@. 
fromBool :: Bool -> Binary
fromBool True  = One
fromBool False = Zero