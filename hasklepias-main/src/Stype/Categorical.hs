{-|
Module      : Stype categorical
Description : Statistical types
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}

{-# LANGUAGE Safe #-}

module Stype.Categorical
  ( module Stype.Categorical.Nominal
  , module Stype.Categorical.Binary
  ) where

import safe           Stype.Categorical.Binary
import safe           Stype.Categorical.Nominal
