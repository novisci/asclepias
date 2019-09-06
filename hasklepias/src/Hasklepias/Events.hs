module Hasklepias.Events(
   Event
 , EventContext
 , event
 , eventContext
 , events
) where

import Hasklepias.Context
import Hasklepias.IntervalAlgebra
import Hasklepias.Events.MedicalDomain
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | TODO

--newtype ProtoEvent a = ProtoEvent (Period, Context a)
-- deriving (Show)

-- | TODO

newtype Event = Event (Period, EventContext)
  deriving (Show)

-- | TODO

event :: Period -> EventContext -> Event
event p c = Event (p, c)


-- | TODO 

type EventDomain  = MedicalDomain
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
