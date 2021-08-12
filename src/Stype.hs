{-|
Module      : Stype 
Description : Statistical types
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}

{-# OPTIONS_HADDOCK hide #-}
-- {-# LANGUAGE Safe #-}

module Stype (
    module Stype.Numeric
  , module Stype.Categorical
  , module Stype.Aeson
) where 

import Stype.Numeric
import Stype.Categorical
import Stype.Aeson