{-|
Module      : Misc types and functions 
Description : Misc types and functions useful in Hasklepias.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

These functions may be moved to more appropriate modules in future versions.
-}
-- {-# OPTIONS_HADDOCK hide #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}


module Hasklepias.Misc
  ( Occurrence(..)
  , CensoringReason(..)
  , OccurrenceReason(..)
  , CensoredOccurrence(..)
  , makeOccurrence
  , getOccurrenceReason
  , getOccurrenceTime
  , adminCensor

   -- ** Manipulating Dates
  , yearFromDay
  , monthFromDay
  , dayOfMonthFromDay

    -- ** Functions for manipulating intervals
  , lookback
  , lookahead

    -- ** Container predicates
  , isNotEmpty

  -- ** Reshaping containers
  , allPairs
  , pairs

  -- ** Other
  , computeAgeAt
  ) where

import           Control.Applicative
import           Data.Time
import           Features.Core                  ( Definition
                                                , Feature
                                                )
import           GHC.Generics                   ( Generic )
import           IntervalAlgebra
import           Stype.Numeric.Censored         ( MaybeCensored(..) )
import           Stype.Numeric.Continuous       ( EventTime )
import qualified Witherable                    as W

-- | A simple typeclass for making a type a "reason" for an event.
class (Ord a, Show a) => OccurrenceReason a where

-- | A type containing the time and when something occurred
newtype Occurrence what when = MkOccurrence ( what , EventTime when )
  deriving (Eq, Show, Generic)

-- | Create an 'Occurrence'
makeOccurrence
  :: (OccurrenceReason what) => what -> EventTime b -> Occurrence what b
makeOccurrence r t = MkOccurrence (r, t)

-- | Get the reason for an 'Occurrence'.
getOccurrenceReason :: Occurrence what b -> what
getOccurrenceReason (MkOccurrence (r, t)) = r

-- | Get the time of an 'Occurrence'.
getOccurrenceTime :: Occurrence what b -> EventTime b
getOccurrenceTime (MkOccurrence (r, t)) = t

-- Define a custom ordering based on times and then reasons.
instance (OccurrenceReason r, Ord b) => Ord (Occurrence r b) where
  compare (MkOccurrence (r1, t1)) (MkOccurrence (r2, t2))
    | t1 < t2              = LT
    | t1 == t2 && r1 < r2  = LT
    | t1 == t2 && r1 == r2 = EQ
    | otherwise            = GT

-- | Sum type for possible censoring and outcome reasons, including administrative
--   censoring.
data CensoringReason cr or = AdminCensor | C cr | O or
  deriving (Eq, Show, Generic)

-- | A type to represent censored 'Occurrence'.
data CensoredOccurrence censors outcomes b = MkCensoredOccurrence
  { reason :: CensoringReason censors outcomes
  , time   :: MaybeCensored (EventTime b)
  }
  deriving (Eq, Generic)

instance (OccurrenceReason c, OccurrenceReason o, Show b) =>
  Show ( CensoredOccurrence c o b ) where
  show (MkCensoredOccurrence r t) = "(" <> show t <> ", " <> show r <> ")"

-- | Creates an administratively censored occurrence.
adminCensor :: EventTime b -> CensoredOccurrence c o b
adminCensor t = MkCensoredOccurrence AdminCensor (RightCensored t)


-- | Compute the "age" in years between two calendar days. The difference between
--   the days is rounded down.
computeAgeAt :: Day -> Day -> Integer
computeAgeAt bd at = floor (fromInteger (diffDays at bd) / 365.25)

-- | Gets the 'Year' from a 'Data.Time.Calendar.Day'.
yearFromDay :: Day -> Year
yearFromDay = (\(y, m, d) -> y) . toGregorian

-- | Gets the 'Data.Time.Calendar.MonthOfDay' from a 'Data.Time.Calendar.Day'.
monthFromDay :: Day -> MonthOfYear
monthFromDay = (\(y, m, d) -> m) . toGregorian

-- | Gets the 'Data.Time.Calendar.DayOfMonth' from a 'Data.Time.Calendar.Day'.
dayOfMonthFromDay :: Day -> DayOfMonth
dayOfMonthFromDay = (\(y, m, d) -> d) . toGregorian

-- | Creates a new @Interval@ of a provided lookback duration ending at the 
--   'begin' of the input interval.
--
-- >>> lookback 4 (beginerval 10 (1 :: Int))
-- (-3, 1)
lookback
  :: (Intervallic i a, IntervalSizeable a b)
  => b   -- ^ lookback duration
  -> i a
  -> Interval a
lookback d x = enderval d (begin x)

-- | Creates a new @Interval@ of a provided lookahead duration beginning at the 
--   'end' of the input interval.
--
-- >>> lookahead 4 (beginerval 1 (1 :: Int))
-- (2, 6)
lookahead
  :: (Intervallic i a, IntervalSizeable a b)
  => b   -- ^ lookahead duration
  -> i a
  -> Interval a
lookahead d x = beginerval d (end x)

-- | Is the input list empty? 
isNotEmpty :: [a] -> Bool
isNotEmpty = not . null

-- | Generate all pair-wise combinations from two lists.
allPairs :: Applicative f => f a -> f b -> f (a, b)
allPairs = liftA2 (,)

-- | Generate all pair-wise combinations of a single list.
pairs :: [a] -> [(a, a)]
-- copied from the hgeometry library (https://hackage.haskell.org/package/hgeometry-0.12.0.4/docs/src/Data.Geometry.Arrangement.Internal.html#allPairs)
-- TODO: better naming differences between pairs and allPairs?
-- TODO: generalize this function over more containers?
pairs = go
 where
  go []       = []
  go (x : xs) = fmap (x, ) xs <> go xs

-- | Create a predicate function that checks whether within a provided spanning
--   interval, are there (e.g. any, all) gaps of (e.g. <, <=, >=, >) a specified
--   duration among  the input intervals?
makeGapsWithinPredicate
  :: ( Monoid (t (Interval a))
     , Monoid (t (Maybe (Interval a)))
     , Applicative t
     , W.Witherable t
     , IntervalSizeable a b
     , Intervallic i0 a
     , IntervalCombinable i1 a
     )
  => ((b -> Bool) -> t b -> Bool)
  -> (b -> b -> Bool)
  -> (b -> i0 a -> t (i1 a) -> Bool)
makeGapsWithinPredicate f op gapDuration interval l =
  maybe False (f (`op` gapDuration) . durations) (gapsWithin interval l)

-- | Within a provided spanning interval, are there any gaps of at least the
--   specified duration among the input intervals?
anyGapsWithinAtLeastDuration
  :: ( IntervalSizeable a b
     , Intervallic i0 a
     , IntervalCombinable i1 a
     , Monoid (t (Interval a))
     , Monoid (t (Maybe (Interval a)))
     , Applicative t
     , W.Witherable t
     )
  => b       -- ^ duration of gap
  -> i0 a  -- ^ within this interval
  -> t (i1 a)
  -> Bool
anyGapsWithinAtLeastDuration = makeGapsWithinPredicate any (>=)

-- | Within a provided spanning interval, are all gaps less than the specified
--   duration among the input intervals?
--
-- >>> allGapsWithinLessThanDuration 30 (beginerval 100 (0::Int)) [beginerval 5 (-1), beginerval 99 10]
-- True
allGapsWithinLessThanDuration
  :: ( IntervalSizeable a b
     , Intervallic i0 a
     , IntervalCombinable i1 a
     , Monoid (t (Interval a))
     , Monoid (t (Maybe (Interval a)))
     , Applicative t
     , W.Witherable t
     )
  => b       -- ^ duration of gap
  -> i0 a  -- ^ within this interval
  -> t (i1 a)
  -> Bool
allGapsWithinLessThanDuration = makeGapsWithinPredicate all (<)
