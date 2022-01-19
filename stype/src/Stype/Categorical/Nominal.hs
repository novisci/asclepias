{-|
Module      : Stype categorical
Description : Statistical types
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}

{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric #-}

module Stype.Categorical.Nominal
  ( Nominal(..)
  ) where

import           GHC.Generics                   ( Generic )

{- | a placeholder for a future nominal type -}
newtype Nominal a = Nominal a deriving ( Eq, Show, Ord, Generic )
