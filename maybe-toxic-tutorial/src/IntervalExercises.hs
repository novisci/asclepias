-- NOTE run ghci with src as current working directory, else you will get a
-- failure to find the examples module

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IntervalExercises (module IntervalExercises) where

-- add imports here as necessary
-- you might need to add those to the .cabal file first
import           Data.Time.Calendar
import           Data.Time.Clock
import           IntervalAlgebra
import           IntervalExamples

   {-
      A WRETCHED DAY OF MEETINGS
      -}

-- CREATING AND MODIFYING INTERVALS

-- EX 1
-- Create a parseMeeting function that appropriately wraps the parseInterval
-- function for the type signature provided below

-- Hint: You can either use pattern matching as in the nutinRight function
-- example to handle the Left, Right cases (within a 'where' statement), or if
-- you are comfortable with the Functor typeclass you can use Either as an
-- instance of that typeclass. See the Either instance list in the base Haskell
-- docs. 
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Either.html#t:Either

parseMeeting :: Hour -> Hour -> Either ParseErrorInterval (Meeting Hour)
parseMeeting = undefined

-- EX 2
-- Create a version of unitInterval called unitMeeting that creates the
-- shortest meeting possible, starting from a given Hour
unitMeeting :: Hour -> Meeting Hour
unitMeeting = undefined

-- BONUS EXERCISE
-- Create a version of unitInterval that is as general as it can be. Hint: look at the type signature of beginerval.
-- NOTE i don't actually think this is doable within the exported
-- interval-algebra api, or at least i don't see a way around the following
-- difficulty: This implementation is the kind of thing i'd lie to write, but
-- it doesn't work because the compiler cannot infer the type of a required by
-- moment, even though we do not actually need to know a for moment. i'm not
-- sure what the issue here is.

--unitInterval' :: IntervalSizeable a b => a -> Interval a
--unitInterval' = beginerval (moment :: b)


-- EX 3
-- Create a function that reschedules a meeting to a different start time,
-- keeping the same duration. to avoid confusion with the reschedule type below, i've called it shiftMeeting
shiftMeeting :: Hour -> Meeting Hour -> Meeting Hour
shiftMeeting = undefined


-- Here i create a type alias called Schedule of a list of MeetingData Hour objects, which could represent the collection of meetings on a given day. It's just to make for easier typing.

-- A reminder about basic differences between newtypes and type aliases: type
-- aliases are defined with the type keyword whereas new types are defined with
-- the newtype keyword. a type alias is essentially a prorammer aid: a type
-- defined as `type Thing = Int` will allow any function using with the type
-- signature Thing -> Thing to accept an Int and vice-versa. newtype Thing =
-- Thing Int defines a constructor function on the right-hand-side of = (also
-- called Thing here), and in addition the compiler does not view Thing as
-- equivalent to Int. See this SO post for the differences between newtype and
-- data declarations.  http://stackoverflow.com/questions/5889696/ddg#5889784


type Schedule = [MeetingData Hour]


-- EX 4
-- Write a function to sort a Schedule by interval start points

sortSchedule :: Schedule -> Schedule
sortSchedule = undefined


-- EX 5
-- Write a function that checks whether any of the meetings in a Schedule have
-- shared support (meaning they overlap in any way)

hasOverlap :: Schedule -> Bool
hasOverlap = undefined


-- EX 6
-- Write a function that takes a list of Hour meeting start times and returns
-- Either String Schedule. This function will try (and possibly fail) to create
-- a Schedule of duration 1 meetings that do not conflict. If none of the Hour
-- start times is duplicated, the function should return Right Schedule, where
-- Schedule is the sorted meeting schedule. Otherwise, it should return Left
-- String, where String is an appropriate error message.

makeBasicSchedule :: [Hour] -> Either String Schedule
makeBasicSchedule = undefined


-- EX 7
-- Create another version of makeBasicSchedule that allows for variable
-- durations, for example by taking [(Integer, Hour)] as input
-- write the type signature for this function yourself.
makeSchedule = undefined


-- EX 8
-- Write a function that checks whether a provided meeting conflicts (i.e. has
-- shared support with) any meeting in Schedule
isConflict :: MeetingData Hour -> Schedule -> Bool
isConflict = undefined


  {-
     EXTRA CHALLENGE

     MEETING IN UTC TIME

     Hour was a fine construct for making a Meeting that represented a single calendar day. But we might wish to have a meeting type that can

     i) represent intervals delineated in any UTC time
     ii) where time blocks are in 15 minute chunks

     Some of the work is already done for you, since interval-alebra already defines the instance IntervalSizeable UTCTime NominalDiffTime

     https://hackage.haskell.org/package/interval-algebra-1.0.0/docs/IntervalAlgebra-Core.html#t:IntervalSizeable

     To learn more about those types

     https://hackage.haskell.org/package/time-1.12/docs/Data-Time-Clock.html

     However, the instance above defines the moment (minimal interval chunk) to be 1e-12 seconds. Not very practical for meetings.

     One solution is to create a wrapper type around NominalDiffTime that has the Meeting-related properties we want, and that allows us to create a new IntervalSizeable instance with a moment of 15 minutes. I don't know if that's the best way to go, but it at least allows us to practice.
      -}



-- NOTE implicitly chunks are relative to the 0 day and hour of the Modified Julian Calendar
-- midnight on 1858-11-17
-- see https://hackage.haskell.org/package/time-1.12/docs/Data-Time-Calendar.html#t:Day

class ChunkSize a where
   chunksizeToNominalDiffTime :: a -> NominalDiffTime

data UTCChunked a
  = UTCChunked
      { chunkSize :: a
      , nChunks   :: Integer
      }
  deriving (Eq)


-- Utilities
-- could write in terms of defaultUTC rather than hard-coding
chunkedToUTCTime :: (ChunkSize a, Eq a) => UTCChunked a -> UTCTime
chunkedToUTCTime cx = addUTCTime dt ref
   where
      ref = UTCTime { utctDay = ModifiedJulianDay 0, utctDayTime = 0 }
      UTCChunked { chunkSize = sx, nChunks = tx } = cx
      dt = chunksizeToNominalDiffTime sx * fromInteger tx

-- EXERCISE
--chunkedFromUTCTime :: (ChunkSize a) => a -> UTCTime -> UTCChunked a
--chunkedFromUTCTime cs t = ???


-- UTCChunked instances
instance (Eq a, ChunkSize a) => Show (UTCChunked a) where
   show = show . chunkedToUTCTime

instance (Eq a, ChunkSize a) => Ord (UTCChunked a) where
   (<=) cx cy = tx <= ty
      where UTCChunked { nChunks = tx } = cx
            UTCChunked { nChunks = ty } = cy

-- Note that chunksSize is required to be the same type.
-- it is up to you to ensure that implies the chunksizes are equal
instance (Eq a, ChunkSize a) => Num (UTCChunked a) where
   (+) cx cy = cx { nChunks = ty + tx }
     where UTCChunked { nChunks = tx } = cx
           UTCChunked { nChunks = ty } = cy
   -- EXERCISE
   (-) = undefined
   (*) = undefined
   abs = undefined
   signum = undefined
   fromInteger = undefined

instance (Eq a, ChunkSize a) => IntervalSizeable (UTCChunked a) NominalDiffTime where
   moment' x  = chunksizeToNominalDiffTime c
      where UTCChunked { chunkSize = c } = begin $ getInterval x

   add x c = c { nChunks = dn + t }
      where
         UTCChunked { chunkSize = s, nChunks = t } = c
         dn = toInteger $ truncate (x / chunksizeToNominalDiffTime s)

   -- EXERCISE
   diff = undefined



-- MAKE STUFF

data FifteenMin = FifteenMin deriving (Show, Eq)
data OneMin = OneMin deriving (Show, Eq)

instance ChunkSize FifteenMin where
   chunksizeToNominalDiffTime _ = fromRational 60 * 15

instance ChunkSize OneMin where
   chunksizeToNominalDiffTime _ = fromRational 60

m1 = UTCChunked { chunkSize = FifteenMin, nChunks = 10 }
m2 = UTCChunked { chunkSize = FifteenMin, nChunks = 20 }
m3 = UTCChunked { chunkSize = OneMin, nChunks = 20 }

good = m1 + m2
-- compile-time error
-- bad = m2 + m3
