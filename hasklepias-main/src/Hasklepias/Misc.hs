{-|
Module      : Misc types and functions
Description : Misc types and functions useful in Hasklepias.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

These functions may be moved to more appropriate modules in future versions.
-}
-- {-# OPTIONS_HADDOCK hide #-}
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

  -- ** Reshaping containers
  , allPairs
  , pairs

  -- ** Other
  , computeAgeAt
  ) where

import           Control.Applicative
import           Data.Time
-- import           IntervalAlgebra
import           EventDataTheory
import           Features.Core            (Definition, Feature)
import           GHC.Generics             (Generic)
import           Stype.Numeric.Censored   (MaybeCensored (..))
import           Stype.Numeric.Continuous (EventTime)
import qualified Witherable               as W

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
