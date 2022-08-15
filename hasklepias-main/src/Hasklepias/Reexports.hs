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
    module GHC.Enum
  , module GHC.Num
  , module GHC.Real
  , module GHC.Generics
  , module GHC.Show
  , module GHC.TypeLits
  , module Control.Monad
  , module Control.Applicative
  , module Data.Bifunctor
  , module Data.Bool
  , module Data.Either
  , module Data.Eq
  , module Data.Int
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Functor.Contravariant
  , module Data.Foldable
  , module Data.Function
  , module Data.List
  , module Data.List.NonEmpty
  , module M
  , module Data.Ord
  , module Data.Proxy
  , module Data.Semigroup
  , module Set
  , module Data.Time.Calendar
  , module Data.Text
  , module Data.Traversable
  , module Data.Tuple
  , module Data.Tuple.Curry
  , module Lens.Micro
  , module Type.Reflection
  , module Safe
  , module Flow
  , module Witherable

    -- ** Re-exports of (potentially) unsafe functions
  , module Data.Aeson
  , module GHC.Exts
  , module GHC.IO
  , module Lens
  , module Test.Tasty
  , module Test.Tasty.HUnit
  , module Text.Pretty.Simple
  , module Witch
  ) where

import           Control.Applicative            (Applicative (..), (<$>))
import           Control.Monad                  (Functor (..), Monad (..),
                                                 filterM, join, mfilter, (=<<),
                                                 (>>=))
import           Data.Bifunctor                 (Bifunctor (..))
import           Data.Bool                      (Bool (..), bool, not,
                                                 otherwise, (&&), (||))
import           Data.Either                    (Either (..))
import           Data.Eq                        (Eq, (==))
import           Data.Foldable                  (Foldable (..), and, asum, or)
import           Data.Function                  (const, flip, id, ($), (.))
import           Data.Functor.Contravariant     (Contravariant (contramap),
                                                 Predicate (..))
import           Data.Int                       (Int (..))
import           Data.List                      (all, any, length, map, null,
                                                 partition, replicate, scanl',
                                                 scanl1, sort, transpose, unzip,
                                                 zip, zipWith, (++))
import           Data.List.NonEmpty             (NonEmpty (..))
import           Data.Map.Strict                as M (Map (..))
import           Data.Maybe                     (Maybe (..), catMaybes,
                                                 fromJust, fromMaybe, isJust,
                                                 isNothing, listToMaybe,
                                                 mapMaybe, maybe, maybeToList)
import           Data.Monoid                    (Monoid (..), mconcat, (<>))
import           Data.Ord                       (Ord (..), Ordering (..), max,
                                                 min)
import           Data.Proxy                     (Proxy (..))
import           Data.Semigroup                 (Semigroup (..))
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
import           Data.Traversable               (Traversable (..))
import           Data.Tuple                     (curry, fst, snd, uncurry)
import           Data.Tuple.Curry               (Curry (..), curryN, uncurryN)
import           Flow                           ((!>), (.>), (<!), (<.), (<|),
                                                 (|>))
import           GHC.Enum                       (Enum (fromEnum))
import           GHC.Generics                   (Generic)
import           GHC.Num                        (Integer (..), Natural (..),
                                                 Num (..), fromInteger)
import           GHC.Real                       (Integral (..), toInteger)
import           GHC.Show                       (Show (..))
import           GHC.TypeLits                   (KnownSymbol (..), symbolVal)
import           Safe                           (headMay, initMay, lastMay,
                                                 maximumMay, minimumMay,
                                                 tailMay)
import           Type.Reflection                (Typeable)
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
import           GHC.Exts                       (IsList (fromList))
import           GHC.IO                         (IO (..))
import           GHC.Natural                    (Natural)
import           Test.Tasty                     hiding (after)
import           Test.Tasty.HUnit
import           Text.Pretty.Simple             (pPrint)

import           Witch

