{-|
Module      : Features
Description : Defines the Feature type and its component types, constructors,
              and class instances
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}
-- {-# OPTIONS_HADDOCK hide #-}

module Features
  (

  -- ** Creating Features
    module Features.Core

  -- ** Adding Attributes to Features
  , module Features.Attributes

  -- ** Exporting Features
  , module Features.Featureable
  , module Features.Featureset
  , module Features.Output
  ) where

import           Features.Attributes
import           Features.Core
import           Features.Featureable
import           Features.Featureset
import           Features.Output
