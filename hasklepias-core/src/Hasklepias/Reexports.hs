{-|
Module      : Hasklepias Types
Description : Re-exports functions from other libraries needed for using
              Hasklepias as a standalone import.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE Safe #-}

module Hasklepias.Reexports
  (

    -- ** Re-exports
    module GHC.Num
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
  , module Set
  , module Data.Time.Calendar
  , module Data.Text
  , module Data.Traversable
  , module Data.Tuple
  , module Data.Tuple.Curry
  , module IntervalAlgebra
  , module IntervalAlgebra.IntervalUtilities
  , module IntervalAlgebra.PairedInterval
  , module Safe
  , module Flow
  , module Witherable
  , setFromList
  , mapFromList
  , mapToList
  ) where

import safe      Control.Applicative            ( (<$>)
                                                , Applicative(..)
                                                )
import safe      Control.Monad                  ( (=<<)
                                                , (>>=)
                                                , Functor(..)
                                                , Monad(..)
                                                , join
                                                )
import safe      Data.Bifunctor                 ( Bifunctor(..) )
import safe      Data.Bool                      ( (&&)
                                                , Bool(..)
                                                , bool
                                                , not
                                                , otherwise
                                                , (||)
                                                )
import safe      Data.Either                    ( Either(..) )
import safe      Data.Eq                        ( (==)
                                                , Eq
                                                )
import safe      Data.Foldable                  ( Foldable(..)
                                                , maximum
                                                , minimum
                                                )
import safe      Data.Function                  ( ($)
                                                , (.)
                                                , const
                                                , flip
                                                , id
                                                )
import safe      Data.Functor.Contravariant     ( Contravariant(contramap)
                                                , Predicate(..)
                                                )
import safe      Data.Int                       ( Int(..) )
import safe      Data.List                      ( (++)
                                                , all
                                                , any
                                                , length
                                                , map
                                                , null
                                                , replicate
                                                , scanl'
                                                , scanl1
                                                , sort
                                                , transpose
                                                , unzip
                                                , zip
                                                , zipWith
                                                )
import safe      Data.List.NonEmpty             ( NonEmpty(..) )
import safe qualified Data.Map.Strict          as M
                                                ( Map(..)
                                                , fromList
                                                , fromListWith
                                                , toList
                                                )
import safe      Data.Maybe                     ( Maybe(..)
                                                , catMaybes
                                                , fromJust
                                                , fromMaybe
                                                , isJust
                                                , isNothing
                                                , listToMaybe
                                                , mapMaybe
                                                , maybe
                                                , maybeToList
                                                )
import safe      Data.Monoid                    ( (<>)
                                                , Monoid(..)
                                                )
import safe      Data.Ord                       ( Ord(..)
                                                , Ordering(..)
                                                , max
                                                , min
                                                )
import safe      Data.Proxy                     ( Proxy(..) )
import safe qualified Data.Set                 as Set
                                                ( Set(..)
                                                , fromList
                                                , member
                                                )
import safe      Data.Text                      ( Text
                                                , pack
                                                )
import safe      Data.Time.Calendar             ( CalendarDiffDays(..)
                                                , Day
                                                , DayOfMonth
                                                , DayOfWeek
                                                , MonthOfYear
                                                , Year
                                                , addGregorianDurationClip
                                                , diffDays
                                                , fromGregorian
                                                , gregorianMonthLength
                                                , toGregorian
                                                )
import safe      Data.Time.Calendar.Quarter     ( Quarter
                                                , QuarterOfYear
                                                , dayQuarter
                                                )
import safe      Data.Traversable               ( Traversable(..) )
import safe      Data.Tuple                     ( curry
                                                , fst
                                                , snd
                                                , uncurry
                                                )
import safe      Data.Tuple.Curry               ( curryN
                                                , uncurryN
                                                )
import safe      GHC.Generics                   ( Generic )
import safe      GHC.Num                        ( Integer(..)
                                                , Natural(..)
                                                , Num(..)
                                                , naturalToInt
                                                )
import safe      GHC.Real                       ( Integral(..)
                                                , toInteger
                                                )
import safe      GHC.Show                       ( Show(..) )
import safe      GHC.TypeLits                   ( KnownSymbol(..)
                                                , symbolVal
                                                )

import safe      Flow                           ( (!>)
                                                , (.>)
                                                , (<!)
                                                , (<.)
                                                , (<|)
                                                , (|>)
                                                )
import           Safe                           ( headMay
                                                , lastMay
                                                )
import safe      Witherable                     ( Filterable(filter)
                                                , Witherable(..)
                                                )


import safe      IntervalAlgebra
import safe      IntervalAlgebra.IntervalUtilities
import safe      IntervalAlgebra.PairedInterval

setFromList :: (Ord a) => [a] -> Set.Set a
setFromList = Set.fromList

mapFromList :: (Ord k) => [(k, a)] -> M.Map k a
mapFromList = M.fromList

mapToList :: (Ord k) => M.Map k a -> [(k, a)]
mapToList = M.toList
