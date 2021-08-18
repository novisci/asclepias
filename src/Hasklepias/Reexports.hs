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
    , module Data.Foldable
    , module Data.Function
    , module Data.List
    , module Data.List.NonEmpty
    , module Data.Ord
    , module Data.Proxy
    , module Set
    , module Data.Time.Calendar 
    , module Data.Text
    , module Data.Tuple

    , module IntervalAlgebra
    , module IntervalAlgebra.IntervalUtilities
    , module IntervalAlgebra.PairedInterval

    , module Safe
    , module Flow
    , module Witherable
) where

import safe GHC.Num                         ( Integer(..)
                                            , Num(..)
                                            , Natural(..) )
import safe GHC.Real                        ( Integral(..), toInteger )
import safe GHC.Generics                    ( Generic )
import safe GHC.Show                        ( Show(..) )
import safe GHC.TypeLits                    ( KnownSymbol(..)
                                            , symbolVal )
import safe Control.Applicative             ( (<$>), Applicative(..) )
import safe Control.Monad                   ( Functor(..)
                                            , Monad(..)
                                            , join
                                            , (>>=)
                                            , (=<<) )
import safe Data.Bifunctor                  ( Bifunctor(..) )
import safe Data.Bool                       ( Bool(..)
                                            , (&&), not, (||)
                                            , bool
                                            , otherwise )
import safe Data.Either                     ( Either(..))
import safe Data.Eq                         ( Eq, (==))
import safe Data.Foldable                   ( Foldable(..)
                                            , minimum
                                            , maximum )
import safe Data.Function                   ( (.), ($), const, id, flip )
import safe Data.Int                        ( Int(..) )
import safe Data.List                       ( all
                                            , any
                                            , map
                                            , length
                                            , null
                                            , zipWith
                                            , replicate
                                            , transpose
                                            , sort
                                            , (++) )
import safe Data.List.NonEmpty              ( NonEmpty(..) )
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
import safe Data.Monoid                     ( Monoid(..), (<>) )
import safe Data.Ord                        ( Ord(..)
                                            , Ordering(..)
                                            , max, min )
import safe Data.Proxy                      ( Proxy(..) )
import safe Data.Set as Set                 ( Set(..), fromList, member)
import safe Data.Time.Calendar              ( Day
                                            , DayOfWeek
                                            , DayOfMonth
                                            , MonthOfYear
                                            , Year
                                            , CalendarDiffDays(..)
                                            , addGregorianDurationClip
                                            , fromGregorian
                                            , toGregorian
                                            , gregorianMonthLength
                                            , diffDays )
import safe Data.Time.Calendar.Quarter      ( QuarterOfYear 
                                            , Quarter
                                            , dayQuarter )
import safe Data.Text                       ( pack, Text )
import safe Data.Tuple                      ( fst, snd, uncurry, curry )

import safe IntervalAlgebra
import safe IntervalAlgebra.IntervalUtilities
import safe IntervalAlgebra.PairedInterval

import safe Witherable                      ( Filterable(filter), Witherable )
import safe Flow                            ( (!>), (.>), (<!), (<.), (<|), (|>) )
import Safe                                 ( headMay, lastMay )
