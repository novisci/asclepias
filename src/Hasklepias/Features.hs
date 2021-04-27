{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-|
Module      : Hasklepias Features
Description : TODO
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

module Hasklepias.Features(

    -- * Types
      Feature(..)
    , MissingReason(..)

    -- * Functions for composing functions
    , safeHead
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
) where

import IntervalAlgebra
    ( IntervalAlgebraic(..), ComparativePredicateOf, Interval
    , IntervalFilterable(..)
    , IntervalRelation(..) )
import Hasklepias.Types.Event
    ( Events, Event
    , hasConcepts, filterEvents, liftIntervalPredicate, liftIntervalFilter
    , ctxt )
import Hasklepias.Types.Context as HC
    ( Concept, HasConcept(hasConcepts), Context )

{- | 
  At this time, a 'Feature' is simply a synonym for @'Either' 'MissingReason' a@, 
  where @a@ can be any type of data derivable from 'Events'.
-}
type Feature a = Either MissingReason a

{- | 
  A 'Feature' may be missing for any number of reasons. 
-}
data MissingReason =
    InsufficientData
  | Excluded
  | Other String
  | Unknown
  deriving (Eq, Read, Show)


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