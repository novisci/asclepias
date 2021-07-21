{-|
Module      : Stype numeric types
Description : Statistical types
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module Stype.Numeric (
    module Stype.Numeric.Count
  , module Stype.Numeric.Continuous
  , module Stype.Numeric.Censored
) where

import safe Stype.Numeric.Count
import safe Stype.Numeric.Continuous
import safe Stype.Numeric.Censored

instance Censorable Double where
instance (Ord a) => Censorable (EventTime a) where
