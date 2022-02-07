{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Planning.Examples.Simple where

import           Data.Text       (Text)
import           IntervalAlgebra
import           qualified Planning.Output as O
import           Planning.Test
import GHC.Natural (naturalFromInteger)


-- dummy for CensoredOccurrence from Hasklepias.Misc
data CensoredOccurrence
  = MkCensoredOccurrence
      { reason :: Text
      , time   :: PairedInterval Text Integer
      }
  deriving (Eq, Show)

-- dummy project-specific event type
newtype ProjectOutcome
  = MkProjectOutcome { groupOne :: CensoredOccurrence }
  deriving (Eq, Show)


-- dummy input type
newtype Event
  = MkEvent (PairedInterval Text Integer)

type Index = Interval Integer

-- data
index :: Index
index = beginervalMoment 0

inputs, inputs' :: [Event]
inputs = map MkEvent [makePairedInterval "is_funny" (beginerval 10 (-1)), makePairedInterval "is_sad" (beginervalMoment 5)]

inputs' = map MkEvent [makePairedInterval "is_lousy" (beginerval 10 (-1)), makePairedInterval "is_sad" (beginervalMoment 5)]

-- cohort builder: just some nonsense
cohortBuilder :: Index -> [Event] -> [ProjectOutcome]
cohortBuilder ix = foldr op []
  where op (MkEvent x) xs = if end (getInterval x) >= end ix 
                               then MkProjectOutcome (MkCensoredOccurrence "after" x) : xs 
                               else xs

-- hygiea instances
instance O.From [ProjectOutcome] O.OutputData where
  from xs = O.MkOutputData tg b
    where tg = map getPairData ixs
          b = map (O.NonNeg . naturalFromInteger . end . getInterval) ixs
          ixs = map (time . groupOne) xs

instance Testable [Event] [ProjectOutcome] where
  toOutput = cohortBuilder index
