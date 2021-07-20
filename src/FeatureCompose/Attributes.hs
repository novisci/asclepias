{-|
Module      : Functions for defining attributes Feature data
Description : Defines attributes instances for Features.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module FeatureCompose.Attributes(
    Attributes(..)
  , HasAttributes(..)
) where

import safe Data.Eq         ( Eq )
import safe Data.Text       ( Text )
import safe GHC.Show        ( Show )
import safe GHC.Generics    ( Generic )

{- |
A data type for holding attritbutes of Features. This type and the @HasAttributes@
are likely to change in future versions.
-}
data Attributes = MkAttributes { 
    getShortLabel :: Text
  , getLongLabel :: Text
  , getDerivation :: Text
  } deriving (Eq, Show, Generic)

class HasAttributes a where
  getAttributes :: a -> Attributes


