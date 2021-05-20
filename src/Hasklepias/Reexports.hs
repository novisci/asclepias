{-|
Module      : Hasklepias Types
Description : Re-exports functions from other libraries needed for using
              Hasklepias as a standalone import.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE Safe #-}

module Hasklepias.Reexports (

    -- * Re-exports
      module Control.Monad
    , module Control.Applicative
    , module Data.Bool
    , module Data.Int
    , module Data.Maybe
    , module Data.Function
    , module Data.Functor
    , module Data.List
    , module Data.Ord
    , module Data.Text
    , module Data.Tuple
    , module Safe
    , module Flow
) where

import safe Control.Monad                   ( (=<<) )
import safe Control.Applicative             ( (<$>) )
import safe Data.Bool                       ( Bool(..)
                                            , (&&), not, (||)
                                            , bool
                                            , otherwise )
import safe Data.Function                   ( (.), ($), const )
import safe Data.Functor                    ( fmap )
import safe Data.Int                        ( Int )
import safe Data.List                       ( all, map, filter, length, null )
import safe Data.Maybe                      ( Maybe(..),
                                              maybe,
                                              isJust,
                                              catMaybes,
                                              fromJust,
                                              fromMaybe,
                                              isNothing,
                                              listToMaybe,
                                              mapMaybe,
                                              maybeToList )
import safe Data.Ord                        ( Ord((>=), (<), (>), (<=))
                                            , max, min )
import safe Data.Time.Calendar              ( Day, fromGregorian, MonthOfYear, Year )
import safe Data.Text                       ( pack, Text )
import safe Data.Tuple                      ( fst, snd, uncurry, curry )
import safe Flow                            ( (!>), (.>), (<!), (<.), (<|), (|>) )
import Safe                                 ( headMay, lastMay )
