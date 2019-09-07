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
import Data.IntMap.Strict as M
--import Data.Sequence (Seq)
--import qualified Data.Sequence as Seq

-- | TODO

--newtype ProtoEvent a = ProtoEvent (Period, Context a)
-- deriving (Show)

-- | TODO

newtype Event = Event { getEvent :: (Period, EventContext) }
  deriving (Show, Eq)

instance Ord Event where 
  (<=) (Event x) (Event y) = fst x <= fst y
  (<)  (Event x) (Event y) = fst x <  fst y
  (>=) x y = not (x < y)
  (>)  x y = not (x <= y)

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

newtype Events = Events (M.IntMap Event)
  deriving (Show)

-- TODO

events :: [Event] -> Events
events l = Events $ M.fromList $ zip [1..length l] l
