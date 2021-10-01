-- NOTE run ghci with src as current working directory, else you will get a
-- failure to find the examples module

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IntervalExercises ( ) where

-- add imports here as necessary
-- you might need to add those to the .cabal file first
import IntervalExamples
import IntervalAlgebra
import Data.Time.Clock
import Data.Time.Calendar


  {-
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


   {- NOTE 
       Below is just some messing around to see if this exercise even really is
       doable. I think it might need to be fleshed out quite a bit as an
       exercise, or greatly simplified.
       -}


-- NOTE implicitly chunks are relative to the 0 day and hour of the Modified Julian Calendar
-- midnight on 1858-11-17
-- see https://hackage.haskell.org/package/time-1.12/docs/Data-Time-Calendar.html#t:Day

data UTCChunked = UTCChunked { chunkSize :: NominalDiffTime, nChunks :: Integer } deriving (Eq)

-- Utilities
chunkComparable :: UTCChunked -> UTCChunked -> Bool
chunkComparable UTCChunked { chunkSize = cx } UTCChunked { chunkSize = cy } = cx == cy

-- TODO is there not an existing constructor from DiffTime?
-- could write in terms of defaultUTC rather than hard-coding
chunkedToUTCTime :: UTCChunked -> UTCTime
chunkedToUTCTime cx = addUTCTime dt ref
   where 
      ref = UTCTime { utctDay = ModifiedJulianDay 0, utctDayTime = 0 }
      UTCChunked { chunkSize = sx, nChunks = tx } = cx
      dt = sx * fromInteger tx
        
chunkedFromUTCTime :: UTCTime -> UTCChunked
chunkedFromUTCTime = undefined


-- UTCChunked instances
instance Show UTCChunked where
   show = show . chunkedToUTCTime

instance Ord UTCChunked where
   (<=) cx cy
     | chunkComparable cx cy = tx <= ty
     | otherwise = undefined
        where UTCChunked { nChunks = tx } = cx
              UTCChunked { nChunks = ty } = cy

-- TODO this is where you really want to enforce chunkSize matching in the type
-- to avoid the undefined behavior and all that boilerplate. you could make a
-- sum type which is, say, FifteenMinute | OneMinute or whatever and use that
-- for chunksize
instance Num UTCChunked where
   (+) cx cy
     | chunkComparable cx cy = cx { nChunks = ty + tx}
     | otherwise = undefined
     where
        UTCChunked { nChunks = tx } = cx
        UTCChunked { nChunks = ty } = cy
   -- TODO
   (-) = undefined
   (*) = undefined
   abs = undefined
   signum = undefined
   fromInteger = undefined

-- TODO again, you want to ensure your interval endpoints have the same
-- chunksize, so that should be part of the type
instance IntervalSizeable UTCChunked NominalDiffTime where
   moment' x  = c
      where UTCChunked { chunkSize = c } = begin $ getInterval x

   add x c = c { nChunks = dn + t }
      where
         UTCChunked { chunkSize = s, nChunks = t } = c
         dn = toInteger $ truncate (x / s)

   diff = undefined


-- Old stuff

-- DEFINE the MeetChunk type based on NominalDiffTime
-- check the docs for the typeclass constraints (left of the =>) for the 'b'
-- type in the IntervalSizeable typeclass. You'll want to derive those
-- typeclasses, along with the Show and Eq typeclasses.

-- newtype MeetChunk =


-- IMPLEMENT the IntervalSizeable instance for UTCTime MeetChunk with a moment of 15 minutes
-- the existing implementation will be a handy reference
-- https://hackage.haskell.org/package/interval-algebra-1.0.0/docs/src/IntervalAlgebra.Core.html#line-792
-- look in the docs for NominalDiffTime to get a handle on units

--instance IntervalSizeable UTCTime MeetChunk where
--   moment = undefined
--   add = undefined
--   diff = undefined
--



-- CREATE a default MeetingData UTCTime 
-- that starts at the current local time whenever this function happens to be
-- run. 

-- to spare some trouble sifting through docs, i've created a default UTCTime
-- you can use

defaultDay :: Day
defaultDay = fromGregorian 2021 10 31

-- 12pm (8 am in EST) on defaultDay
defaultUTC :: UTCTime
defaultUTC = UTCTime defaultDay (fromInteger (12 * 60 ^ 2))

defaultMeeting :: MeetingData UTCTime
defaultMeeting = undefined



-- TODO add exercises

