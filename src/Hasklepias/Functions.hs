{-|
Module      : Hasklepias Feature building functions 
Description : Functions for composing features. 
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module Hasklepias.Functions(

    -- * Functions for composing functions
      safeHead
    , safeLast
    , isNotEmpty
    , makeConceptsFilter
    , nthConceptOccurrence
    , firstConceptOccurrence
    , atleastNofX
    , twoXOrOneY
    , hasConcept
    , hasConcepts
    , filterEvents
    , liftIntervalPredicate
    , lift2IntervalPredicate
    , liftIntervalFilter
    , makeEventFilter
    , makePairedFilter
    , allPairs
    , splitByConcepts
) where

import IntervalAlgebra
    ( Intervallic(..)
    , IntervalAlgebraic(..)
    , ComparativePredicateOf
    , IntervalSizeable(..)
    , Interval
    , IntervalRelation(..) )
import IntervalAlgebra.PairedInterval
import IntervalAlgebra.IntervalUtilities
import Hasklepias.Types.Event( Events, Event, ctxt, ConceptEvent )
import Hasklepias.Types.Context as HC
    ( Concept, Concepts, HasConcept(hasConcept, hasConcepts), Context )
import Data.Text(Text)
import Control.Applicative

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
makeConceptsFilter ::
       [Text] -- ^ the list of concepts by which to filter 
    -> Events a
    -> Events a
makeConceptsFilter cpts = filterEvents (`hasConcepts` cpts)

-- | Filter 'Events' to a single @'Maybe' 'Event'@, based on a provided function,
--   with the provided concepts. For example, see 'firstConceptOccurrence' and
--  'lastConceptOccurrence'.
nthConceptOccurrence ::
       (Events a -> Maybe (Event a)) -- ^ function used to select a single event
    -> [Text]
    -> Events a
    -> Maybe (Event a)
nthConceptOccurrence f c = f.makeConceptsFilter c

-- | Finds the *first* occurrence of an 'Event' with at least one of the concepts.
--   Assumes the input 'Events' list is appropriately sorted.
firstConceptOccurrence ::
      [Text]
    -> Events a
    -> Maybe (Event a)
firstConceptOccurrence = nthConceptOccurrence safeHead

-- | Finds the *last* occurrence of an 'Event' with at least one of the concepts.
--   Assumes the input 'Events' list is appropriately sorted.
lastConceptOccurrence ::
      [Text]
    -> Events a
    -> Maybe (Event a)
lastConceptOccurrence = nthConceptOccurrence safeLast

-- | Does 'Events' have at least @n@ events with any of the Concept in @x@.
atleastNofX ::
      Int -- ^ n
   -> [Text] -- ^ x
   -> Events a -> Bool
atleastNofX n x es = length (makeConceptsFilter x es) >= n

-- | TODO
twoXOrOneY :: [Text] -> [Text] -> Events a -> Bool
twoXOrOneY x y es = atleastNofX 2 x es ||
                    atleastNofX 1 y es


-- | Filter @Events a@ by a predicate function
filterEvents ::
    (Event a -> Bool)
    -> Events a
    -> Events a
filterEvents = filter

-- | TODO
liftIntervalPredicate :: (Ord a, Show a) =>
    ComparativePredicateOf (Interval a)
    -> Interval a
    -> Event a
    -> Bool
liftIntervalPredicate f x y = f x (getInterval y)

-- | TODO
lift2IntervalPredicate ::  (Ord a, Show a) =>
       ComparativePredicateOf (Interval a)
    -> ComparativePredicateOf (Event a)
lift2IntervalPredicate f x y = f (getInterval x) (getInterval y)


-- | TODO
makeEventFilter ::  (Ord a, Show a) =>
       ComparativePredicateOf (Interval a) -- ^ an 'IntervalAlgebraic' predicate
    -> Interval a -- ^ an interval to compare to intervals in the input eventsa
    -> (Context -> Bool) -- ^ predicate on a 'Context'
    -> Events a
    -> Events a
makeEventFilter fi i fc = filterEvents (\x -> liftIntervalPredicate fi i x &&
                                              fc (ctxt x) )


-- | Takes a predicate of intervals and a predicate on the data part of a 
--   paired interval to create a single predicate such that both input
--   predicates should hold.
makePairPredicate' :: (IntervalAlgebraic (PairedInterval b) a
                     , IntervalAlgebraic Interval a
                     , IntervalAlgebraic i0 a) =>
       ComparativePredicateOf (Interval a)
    -> i0 a
    -> (b -> Bool)
    -> (PairedInterval b a -> Bool)
makePairPredicate' pi i pd x = compareIntervals pi i x && pd (getPairData x)


makePairedFilter :: ( IntervalAlgebraic Interval a
                     , IntervalAlgebraic i0 a
                     , IntervalAlgebraic (PairedInterval b) a
                     ) =>
               ComparativePredicateOf (Interval a)
            -> i0 a
            -> (b -> Bool)
            -> [PairedInterval b a]
            -> [PairedInterval b a]
makePairedFilter fi i fc = filter (makePairPredicate' fi i fc)

-- | Lifts a 'Interval' predicate to create a filter of events.
liftIntervalFilter ::  (Ord a, Show a) =>
       ComparativePredicateOf (Interval a) -- ^ an 'IntervalAlgebraic' predicate
    -> Interval a -- ^ an interval to compare to intervals in the input events
    -> Events a
    -> Events a
liftIntervalFilter f i = makeEventFilter f i (const True)

-- | Generate all pair-wise combinations from two lists.
allPairs :: [a] -> [a] -> [(a, a)]
allPairs = liftA2 (,)


-- | Split an @Events a@ into a pair of @Events a@. The first element contains
--   events have any of the concepts in the first argument, similarly for the
--   second element.
splitByConcepts :: [Text] 
        -> [Text]
        -> Events a
        -> (Events a, Events a)
splitByConcepts c1 c2 es = ( filter (`hasConcepts` c1) es
                           , filter (`hasConcepts` c2) es)