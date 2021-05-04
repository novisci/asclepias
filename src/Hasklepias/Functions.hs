{-|
Module      : Hasklepias Feature building functions 
Description : Functions for composing features. 
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

module Hasklepias.Functions(

    -- * Functions for composing functions
      safeHead
    , safeLast
    , isNotEmpty
    , makeConceptsFilter
    , nthConceptOccurrence
    , firstConceptOccurrence
    , withinFilter
    , overContainment
    , overFilter
    , atleastNofX
    , twoXOrOneY
    , intervals
    , hasConcept
    , hasConcepts
    , filterEvents
    , liftIntervalPredicate
    , lift2IntervalPredicate
    , liftIntervalFilter
    , makeEventFilter
) where

import IntervalAlgebra
    ( IntervalAlgebraic(..), ComparativePredicateOf, Interval
    , IntervalRelation(..) )
import Hasklepias.Types.Event( Events, Event, intrvl, ctxt )
import Hasklepias.Types.Context as HC
    ( Concept, HasConcept(hasConcept, hasConcepts), Context )


-- | Safely gets the 'head' of a list.
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

-- | Safely get the 'last' of a list.
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast x  = Just $ last x

-- | Is the input list empty? 
isNotEmpty :: [a] -> Bool
isNotEmpty = not.null

-- | Filter 'Events' to those that have any of the provided concepts.
makeConceptsFilter :: (IntervalAlgebraic a) =>
       [Concept] -- ^ the list of concepts by which to filter 
    -> Events a
    -> Events a
makeConceptsFilter cpts = filterEvents (`hasConcepts` cpts)

-- | Filter 'Events' to a single @'Maybe' 'Event'@, based on a provided function,
--   with the provided concepts. For example, see 'firstConceptOccurrence' and
--  'lastConceptOccurrence'.
nthConceptOccurrence :: (IntervalAlgebraic a) =>
       (Events a -> Maybe (Event a)) -- ^ function used to select a single event
    -> [Concept]
    -> Events a
    -> Maybe (Event a)
nthConceptOccurrence f c = f.makeConceptsFilter c

-- | Finds the *first* occurrence of an 'Event' with at least one of the concepts.
--   Assumes the input 'Events' list is appropriately sorted.
firstConceptOccurrence :: (IntervalAlgebraic a) =>
      [Concept]
    -> Events a
    -> Maybe (Event a)
firstConceptOccurrence = nthConceptOccurrence safeHead

-- | Finds the *last* occurrence of an 'Event' with at least one of the concepts.
--   Assumes the input 'Events' list is appropriately sorted.
lastConceptOccurrence :: (IntervalAlgebraic a) =>
      [Concept]
    -> Events a
    -> Maybe (Event a)
lastConceptOccurrence = nthConceptOccurrence safeLast

-- | Does 'Events' have at least @n@ events with any of the Concept in @x@.
atleastNofX :: (IntervalAlgebraic a) => 
      Int -- ^ n
   -> [Concept] -- ^ x
   -> Events a -> Bool
atleastNofX n x es = length (makeConceptsFilter x es) >= n

-- | TODO
twoXOrOneY :: (IntervalAlgebraic a) => [Concept] -> [Concept] -> Events a -> Bool
twoXOrOneY x y es = atleastNofX 2 x es ||
                    atleastNofX 1 y es

-- | TODO
withinFilter :: (IntervalAlgebraic a) => Interval a -> Events a -> Events a
withinFilter = liftIntervalFilter within

-- | TODO
overContainment :: IntervalAlgebraic a => ComparativePredicateOf (Interval a)
overContainment = predicate $ toSet [Contains, StartedBy, FinishedBy, Equals]

-- | TODO
overFilter :: IntervalAlgebraic a => Interval a -> Events a -> Events a
overFilter = liftIntervalFilter overContainment 

-- | Filter @Events a@ by a predicate function
filterEvents :: (IntervalAlgebraic a) =>
    (Event a -> Bool)
    -> Events a
    -> Events a
filterEvents = filter

-- | TODO
liftIntervalPredicate :: (IntervalAlgebraic a) =>
    ComparativePredicateOf (Interval a)
    -> Interval a
    -> Event a
    -> Bool
liftIntervalPredicate f x y = f x (intrvl y)

-- | TODO
lift2IntervalPredicate :: (IntervalAlgebraic a) =>
       ComparativePredicateOf (Interval a)
    -> ComparativePredicateOf (Event a)
lift2IntervalPredicate f x y = f (intrvl x) (intrvl y)

-- | Extracts the interval part of each 'Event' into a list of intervals.
intervals :: Events a -> [Interval a]
intervals = map intrvl

-- | TODO
makeEventFilter :: (IntervalAlgebraic a) =>
       ComparativePredicateOf (Interval a) -- ^ an 'IntervalAlgebraic' predicate
    -> Interval a -- ^ an interval to compare to intervals in the input eventsa
    -> (Context -> Bool) -- ^ predicate on a 'Context'
    -> Events a
    -> Events a
makeEventFilter fi i fc = filterEvents (\x -> liftIntervalPredicate fi i x && 
                                              fc (ctxt x) )

-- | Lifts a 'Interval' predicate to create a filter of events.
liftIntervalFilter :: (IntervalAlgebraic a) =>
       ComparativePredicateOf (Interval a) -- ^ an 'IntervalAlgebraic' predicate
    -> Interval a -- ^ an interval to compare to intervals in the input events
    -> Events a
    -> Events a
liftIntervalFilter f i = makeEventFilter f i (const True)



