{-|
Module      : Hasklepias Features
Description : TODO
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

module Hasklepias.Types.Feature(
    -- * Types
      Feature(..)
    , MissingReason(..)

) where

{- | A 'Feature' is simply a synonym for @'Either' 'MissingReason' d@, where 
  @d@ can be any type of data derivable from 'Events'.
-}
type Feature d = Either MissingReason d

-- | A 'Feature' may be missing for any number of reasons. 
data MissingReason =
    InsufficientData
  | Excluded
  | Other String
  | Unknown
  deriving (Eq, Read, Show)
