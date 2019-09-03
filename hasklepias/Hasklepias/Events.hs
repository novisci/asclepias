module Hasklepias.Events(
   Event
 , EventDomain(..)
 , EventContext
 , event
 , eventContext
 , events
) where

import Hasklepias.Context
import Hasklepias.IntervalAlgebra
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | TODO

newtype ProtoEvent a = ProtoEvent (Period, Context a)
  deriving (Show)

-- | TODO

newtype Event = Event (Period, EventContext)
  deriving (Show)

-- | TODO

event :: Period -> EventContext -> Event
event p c = Event (p, c)

-- | TODO 

data EventDomain = 
    Lab  
    { getLoinc :: String, getValue :: Float, getUnits :: String }
  | Diagnosis 
    { getLocation :: String, getCode :: String , getCodebook :: String }
  | Enrollment 
    { getPlan :: String }
  deriving (Show)

-- | TODO 

type EventContext = Context EventDomain


-- | TODO

eventContext :: EventDomain -> Source -> EventContext
eventContext = Context


-- TODO

newtype Events = Events (Seq Event)
  deriving (Show)

-- TODO

events :: [Event] -> Events
events l = Events $ Seq.fromList l
