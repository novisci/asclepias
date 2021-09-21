{-|
Module      : Stype censored types and typeclasses
Description : Statistical types
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Stype.Numeric.Censored (
    Censorable(..)
  , MaybeCensored(..)
  , ParseIntervalError(..)
) where

import safe Data.Text                  
  -- ( Text, pack, unpack )
import safe GHC.Generics                 ( Generic )

-- | Data for censored data
data MaybeCensored a where
  IntervalCensored :: a -> a -> MaybeCensored a
  RightCensored :: a -> MaybeCensored a
  LeftCensored :: a -> MaybeCensored a
  Uncensored :: a -> MaybeCensored a
  deriving( Eq, Show, Ord, Generic )

-- | A type to hold a reason that interval fails to parse.
newtype ParseIntervalError = ParseIntervalError Text
  deriving ( Eq, Show)

-- | A class to censor data
class (Ord a, Show a) => Censorable a where
 
  parseIntervalCensor :: a -> a -> Either ParseIntervalError (MaybeCensored a)
  parseIntervalCensor x y
    | x < y = Right $ IntervalCensored x y
    | otherwise = Left $ ParseIntervalError ( pack (show y ++ " < " ++ show x) )

  rightCensor ::  a -> a -> MaybeCensored a
  rightCensor c t
    | c < t     = RightCensored c
    | otherwise = Uncensored t

  leftCensor ::  a -> a -> MaybeCensored a
  leftCensor c t
    | c >= t    = LeftCensored c
    | otherwise = Uncensored t