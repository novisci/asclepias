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
{-# LANGUAGE MultiParamTypeClasses #-}

module FeatureCompose.Attributes(
      Attributes(..)
    , Role(..)
    , Purpose(..)
    , HasAttributes(..)
    , emptyAttributes
    , basicAttributes
    , emptyPurpose
) where

import safe Data.Eq         ( Eq )
import safe Data.Ord        ( Ord )
import safe Data.Set        ( Set, empty, fromList )
import safe Data.Text       ( Text )
import safe GHC.Show        ( Show )
import safe GHC.Generics    ( Generic )
import safe GHC.TypeLits    ( KnownSymbol )

{- | A type to identify a feature's role -}
data Role =
    Outcome
  | Covariate
  | Exposure
  | Competing
  | Weight
  | Intermediate
  | Unspecified
  deriving (Eq, Ord, Show, Generic)

{- | A type to identify a feature's purpose -}
data Purpose = MkPurpose {
      getRole :: Set Role 
    , getTags :: Set Text
 } deriving (Eq, Show, Generic)

{- |
A data type for holding attritbutes of Features. This type and the @HasAttributes@
are likely to change in future versions.
-}
data Attributes = MkAttributes { 
    getShortLabel :: Text
  , getLongLabel :: Text
  , getDerivation :: Text
  , getPurpose :: Purpose
  } deriving (Eq, Show, Generic)

{- | Just empty purpose -}
emptyPurpose :: Purpose
emptyPurpose = MkPurpose empty empty

{- | Just empty attributes -}
emptyAttributes :: Attributes
emptyAttributes = MkAttributes "" "" "" emptyPurpose

{- | Create attributes with just short label, long label, roles, and tags. -}
basicAttributes :: 
     Text -- ^ short label
  -> Text -- ^ long label
  -> [Role] -- ^ purpose roles
  -> [Text] -- ^ purpose tags
  -> Attributes
basicAttributes sl ll rls tgs = 
  MkAttributes sl ll "" 
  (MkPurpose (fromList rls) (fromList tgs))


class (KnownSymbol n) => HasAttributes n a where
  getAttributes :: f n a -> Attributes
  getAttributes _ = emptyAttributes
