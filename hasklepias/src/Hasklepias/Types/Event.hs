{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Hasklepias Event Type
Description : Specifies the Event type and related functions
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

module Hasklepias.Types.Event(
   Event
 -- , EventContext
 -- , Events
 , getEvent
 , event
 , hasConcept
 , hasConcepts
 -- , eventContext
-- , events
 , filterEvents
 , liftIntervalPredicate
 , lift2IntervalPredicate

 , combineIntervals
 , gaps

) where

import IntervalAlgebra
import Hasklepias.Types.Context
import Data.Semigroup
import Data.Maybe (catMaybes)

-- | An Event @a@ is simply a pair @(Interval a, Context)@
newtype Event a =  Event { getEvent :: (Interval a, Context) }
  deriving (Eq)

instance (Intervallic a) => Ord (Event a) where 
  (<=) (Event x) (Event y) = fst x <= fst y
  (<)  (Event x) (Event y) = fst x <  fst y
  (>=) x y = not (x < y)
  (>)  x y = not (x <= y)

instance (Intervallic a, Show a) => Show (Event a) where 
  show x = "{" ++ show (fst $ getEvent x) ++ ", " ++ 
                  show (snd $ getEvent x) ++ "}"

instance HasConcept (Event a) where
    hasConcept x y = (snd $ getEvent x) `hasConcept` y

-- | A smart constructor for 'Event a's.
event :: (IntervalAlgebraic a) => Interval a -> Context -> Event a
event i c = Event (i, c)

-- | Access the 'Interval a' of an 'Event a'.
intrvl :: Event a -> Interval a
intrvl = fst.getEvent

-- | Access the 'Context' of an 'Event a'.
ctxt :: Event a -> Context
ctxt = snd.getEvent

-- | A @List@ of @Event a@
-- 
-- NOTE (20190911): I (B. Saul) am starting out the Events type as a 
-- list of the Event type. This may be not be the optimal approach,
-- especially with regards to lookup/filtering the list. Ideally,
-- we could do one pass through the ordered container (whatever it is)
-- to identify events by concept; rather than repeated evaluations of
-- the lookup predicates. This could be handled by, for example, 
-- representing Events has a Map with a list of concept indices. 
-- But it gets us off the ground.
type Events a = [Event a]

-- | Filter @Events a@ by a predicate function
filterEvents :: (IntervalAlgebraic a) => 
                (Event a -> Bool) 
                -> Events a 
                -> Events a
filterEvents = filter

--
liftIntervalPredicate :: (IntervalAlgebraic a) => 
                ComparativePredicateOf (Interval a)
                -> Interval a
                -> Event a
                -> Bool
liftIntervalPredicate f = (\x y -> ( f x (intrvl y) ))

--
lift2IntervalPredicate :: (IntervalAlgebraic a) => 
                ComparativePredicateOf (Interval a)
                -> ComparativePredicateOf (Event a)
lift2IntervalPredicate f = (\x y -> ( f (intrvl x) (intrvl y) ))

-- | Create a new @Interval a@ from the beginning of @x@ and end of @y@. This 
-- assumes 'x <= y'!
extent :: (Intervallic a) => Interval a -> Interval a -> Interval a
extent x y = unsafeInterval (begin x) (end y)

-- | Box to avoid overlapping instances
newtype Box a = Box { unBox :: [a] }
instance (IntervalAlgebraic a) => Semigroup (Box (Interval a)) where 
    Box x <> Box y
       | null x         = Box y
       | null y         = Box x
       | lx `before` fy = Box $ x ++ y
       | otherwise      = Box $ init x ++ [extent lx fy] ++ tail y
       where lx = last x
             fy = head y


-- | combineIntervals
combineIntervals :: (IntervalAlgebraic a) => 
                    [Interval a]
                    -> [Interval a]
combineIntervals l = unBox $ foldr ((<>) . (\z -> Box [z])) (Box []) l


-- | If 'x' is 'before' 'y', form a new @Just Interval a@ from the 'end' of 'x' 
-- and the 'begin' of 'y'. Otherwise, 'Nothing'.
(><) :: (IntervalAlgebraic a) => Interval a -> Interval a -> Maybe (Interval a)
(><) x y 
    | x `before` y = Just $ unsafeInterval (end x) (begin y)
    | otherwise    = Nothing

-- |
gaps :: (IntervalAlgebraic a) => [Interval a] -> [Interval a] 
gaps l = catMaybes $ map (\x -> (fst x) >< (snd x)) $ (zip <*> tail) l 

