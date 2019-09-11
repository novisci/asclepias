module Hasklepias.Events(
   Event
 , EventContext
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

type Events = [Event]

filterEvents ::  Events -> (Event -> Bool) -> Events
filterEvents e f = filter f e

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