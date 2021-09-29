-- NOTE run ghci with src as current working directory, else you will get a
-- failure to find the examples module

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IntervalExercises ( ) where

-- add imports here as necessary
-- you might need to add those to the .cabal file first
import IntervalExamples
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

dayDefault :: Day
dayDefault = fromGregorian 2021 10 31

-- 12pm (8 am in EST) on dayDefault
utcDefault :: UTCTime
utcDefault = UTCTime dayDefault (fromInteger (8 * 60 ^ 2))

defaultMeeting :: MeetingData UTCTime
defaultMeeting = undefined



-- TODO add exercises

