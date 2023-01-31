{-|
Module      : Hasklepias Types
Description : Re-exports functions from other libraries needed for using
              Hasklepias as a standalone import.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# OPTIONS_HADDOCK hide #-}

module Hasklepias.Reexports
  (

    -- ** Re-exports
  module M
  , module Set
  , module Data.Time.Calendar
  , module Data.Text
  , module Lens.Micro
  , module Safe
  , module Flow
  , module Witherable

    -- ** Re-exports of (potentially) unsafe functions
  , module Data.Aeson
  , module Lens
  , module Test.Tasty
  , module Test.Tasty.HUnit
  , module Text.Pretty.Simple
  , module Witch
  ) where

import           Data.Map.Strict                as M (Map (..), findWithDefault,
                                                      lookup, (!?))
import           Data.Set                       as Set (Set (..), empty, member)
import           Data.Text                      (Text, pack)
import           Data.Time.Calendar             (CalendarDiffDays (..), Day,
                                                 DayOfMonth, DayOfWeek,
                                                 MonthOfYear, Year,
                                                 addGregorianDurationClip,
                                                 diffDays, fromGregorian,
                                                 gregorianMonthLength,
                                                 toGregorian)
import           Data.Time.Calendar.Quarter     (Quarter, QuarterOfYear,
                                                 dayQuarter)
import           Flow                           ((!>), (.>), (<!), (<.), (<|),
                                                 (|>))
import           Safe                           (headMay, initMay, lastMay,
                                                 maximumMay, minimumMay,
                                                 tailMay)
import           Witherable                     (Filterable (filter),
                                                 Witherable (..))


import           Lens.Micro                     ((^?))


import           Data.Aeson                     (FromJSON (..), Options (..),
                                                 SumEncoding (..), ToJSON (..),
                                                 defaultOptions, encode,
                                                 genericParseJSON)
import           Data.Generics.Internal.VL.Lens as Lens ((^.))
import           Data.Generics.Product          as Lens (HasField (field))
import           Data.Generics.Sum              as Lens (AsAny (_As))
import           Test.Tasty                     hiding (after)
import           Test.Tasty.HUnit
import           Text.Pretty.Simple             (pPrint)

import           Witch

