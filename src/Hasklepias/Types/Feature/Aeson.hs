{-|
Module      : Functions for encoding Hasklepias Feature data
Description : Defines ToJSON instances for Features.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Hasklepias.Types.Feature.Aeson(
) where

import IntervalAlgebra
import GHC.Generics
import Hasklepias.Types.Feature ( MissingReason, Feature(..) )
import Data.Aeson
--  ( ToJSON, toJSON )

instance (ToJSON a, Ord a, Show a)=> ToJSON (Interval a) where
    toJSON x = 
        object ["begin" .= begin x, "end" .= end x]
instance ToJSON MissingReason 
instance (ToJSON d)=> ToJSON (Feature d) where
    toJSON (Feature x) = case x of 
      (Left l)  -> toJSON l
      (Right r) -> toJSON r

