{-|
Module      : Stype numeric types
Description : Statistical types
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}

{-# LANGUAGE Safe #-}

module Stype.Numeric
  ( module Stype.Numeric.Count
  , module Stype.Numeric.Continuous
  , module Stype.Numeric.Censored
  ) where

import safe           Stype.Numeric.Censored
import safe           Stype.Numeric.Continuous
import safe           Stype.Numeric.Count

instance Censorable Double where
instance (Ord a, Show a) => Censorable (EventTime a) where
