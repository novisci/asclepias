module Hasklepias.Events(
   Event
 , EventContext
 , Events
 , getEvent
 , event
 , eventContext
-- , events
 , hasConcept
 , filterEvents
) where

import Hasklepias.Context
import Hasklepias.IntervalAlgebra
import Hasklepias.Context.ClaimsDomain
--import Data.IntMap.Strict as M

-- | TODO

newtype Event = Event { getEvent :: (Period, EventContext) }
  deriving (Eq)

instance Ord Event where 
  (<=) (Event x) (Event y) = fst x <= fst y
  (<)  (Event x) (Event y) = fst x <  fst y
  (>=) x y = not (x < y)
  (>)  x y = not (x <= y)

instance Show Event where 
  show x = "{" ++ show (fst $ getEvent x) ++ ", " ++ show (snd $ getEvent x) ++ "}"

-- | TODO 

type EventDomain  = Maybe ClaimsDomain
type EventContext = Context EventDomain


-- | TODO

event :: Period -> EventContext -> Event
event p c = Event (p, c)

-- | TODO

eventContext :: Maybe [String] -> Maybe EventDomain -> Source -> EventContext
eventContext = Context

-- | TODO
hasConcept' :: String -> (EventContext -> Bool)
hasConcept' name = 
    \c -> 
    case (getConcepts c) of 
        Just concepts -> name `elem` concepts
        Nothing -> False

hasConcept :: String -> (Event -> Bool)
hasConcept name = (\x -> hasConcept' name $ snd $ getEvent x)


-- | TODO
-- NOTE (20190911): I (B. Saul) am starting out the Events type as a 
-- list of the Event type. This may be not be the optimal approach,
-- especially with regards to lookup/filtering the list. Ideally,
-- we could do one pass through the ordered container (whatever it is)
-- to identify events by concept; rather than repeated evaluations of
-- the lookup predicates. This could be handled by, for example, 
-- representing Events has a Map with a list of concept indices. 
-- But it gets us off the ground.

type Events = [Event]

filterEvents :: (Event -> Bool) -> Events -> Events
filterEvents = filter

{-
newtype Events = Events (M.IntMap Event)
  deriving (Show)

-- | TODO

events :: [Event] -> Events
events l = Events $ M.fromList $ zip [1..length l] l

-- | TODO
filterEvents ::  Events -> (Event -> Bool) -> Events
filterEvents (Events m) f = Events $ M.filter f m
-}