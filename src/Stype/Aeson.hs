{-|
Module      : Stype aeson instances
Description : Statistical types
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}

{-# LANGUAGE DeriveGeneric #-}

module Stype.Aeson (
  Count(..)
) where

import Data.Aeson
import Stype.Numeric

instance ToJSON Count where