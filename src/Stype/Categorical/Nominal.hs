{-|
Module      : Stype categorical
Description : Statistical types
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}
-- {-# LANGUAGE Safe #-}
module Stype.Categorical.Nominal (
  Nomimal(..)
) where

-- import qualified Data.Set.NonEmpty as NES

{- | a placeholder for a future nominal type -}
newtype Nomimal a = Nomimal a deriving ( Eq, Show, Ord )