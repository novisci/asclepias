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

module Stype.Numeric.Censored (
    Censorable(..)
  , MaybeCensored(..)
) where

import safe Data.Text                    (Text)

data MaybeCensored a where
  IntervalCensored :: a -> a -> MaybeCensored a
  RightCensored :: a -> MaybeCensored a
  LeftCensored :: a -> MaybeCensored a
  Uncensored :: a -> MaybeCensored a
  deriving( Eq, Show, Ord )

class (Ord a) => Censorable a where

  parseIntervalCensor :: a -> a -> Either Text (MaybeCensored a)
  parseIntervalCensor x y 
    | x < y = Right $ IntervalCensored x y
    | otherwise = Left "y >= x"

  rightCensor ::  a -> a -> MaybeCensored a
  rightCensor c t
    | c < t     = RightCensored c 
    | otherwise = Uncensored t

  leftCensor ::  a -> a -> MaybeCensored a
  leftCensor c t
    | c >= t    = LeftCensored c
    | otherwise = Uncensored t