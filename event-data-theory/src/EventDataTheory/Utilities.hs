{-# LANGUAGE TupleSections #-}
{-|
Module      : Misc utilities useful in working with events 
-}

module EventDataTheory.Utilities
  ( (|||)
  , (&&&)
  , Predicate(..)
  {-
  RE: haddock message:
    80% (  4 /  5) in 'EventDataTheory.Utilities'
    Missing documentation for:
      Predicate
  See: https://github.com/haskell/haddock/issues/958
  -}
  , containsConcepts
  , findOccurrenceOfEvent
  , firstOccurrenceOfConcept
  , lastOccurrenceOfConcept
  , splitByConcepts
  , filterEvents
  , tallyEvents
  ) where

import           Data.Foldable                  ( length
                                                , toList
                                                )
import           Data.Functor.Contravariant     ( Predicate(..) )
import           Data.Semigroup
import           EventDataTheory.Core
import           IntervalAlgebra
import           Safe                           ( headMay
                                                , lastMay
                                                )
import qualified Witherable                    as W

-- | Combine two 'Predicate's by "or".
(|||) :: Predicate a -> Predicate a -> Predicate a
(|||) f g =
  Predicate (\x -> getAny (Any (getPredicate f x) <> Any (getPredicate g x)))

-- | Combine two 'Predicate's by "and".
(&&&) :: Predicate a -> Predicate a -> Predicate a
(&&&) f g =
  Predicate (\x -> getAll (All (getPredicate f x) <> All (getPredicate g x)))

{-| 
Creates a predicate to check that an 'Event' contains
any of a given set of concepts.
-}
containsConcepts :: (Ord c) => [c] -> Predicate (Event c m a)
containsConcepts cpt = Predicate (`hasAnyConcepts` cpt)

{-|
Filter a container of events by a predicate.
-}
filterEvents
  :: (W.Filterable f)
  => Predicate (Event c m a)
  -> f (Event c m a)
  -> f (Event c m a)
filterEvents p = W.filter (getPredicate p)

{-|
Filter a container of 'Event's 
to a single @'Maybe' 'Event'@,
based on a provided function,
with the provided concepts. 

For example,
see 'firstOccurrenceOfConcept' and
'lastOccurrenceOfConcept'.
-}
findOccurrenceOfEvent
  :: (W.Filterable f)
  => (f (Event c m a) -> Maybe (Event c m a)) -- ^ function used to select a single event after the container is filtered
  -> Predicate (Event c m a) -- ^ predicate by which to filter
  -> f (Event c m a) -- ^ a container of events
  -> Maybe (Event c m a)
findOccurrenceOfEvent f p = f . filterEvents p

{-|
Finds the *first* occurrence of an 'Event' with at least one of the concepts
if one exists.
Assumes the input events are appropriately sorted.
-}
firstOccurrenceOfConcept
  :: (W.Witherable f, Ord c) => [c] -> f (Event c m a) -> Maybe (Event c m a)
firstOccurrenceOfConcept x =
  findOccurrenceOfEvent (headMay . toList) (containsConcepts x)

{-|
Finds the *last* occurrence of an 'Event' with at least one of the concepts
if one exists.
Assumes the input events list are appropriately sorted.
-}
lastOccurrenceOfConcept
  :: (W.Witherable f, Ord c) => [c] -> f (Event c m a) -> Maybe (Event c m a)
lastOccurrenceOfConcept x =
  findOccurrenceOfEvent (lastMay . toList) (containsConcepts x)

{-|
Split a container of @'Event'@s 
into a pair of @'Event'@s. 
The first element contains
events have any of the concepts in the first argument,
similarly for the second element.

Note that one or both of the resulting containers
may be empty.
-}
splitByConcepts
  :: (W.Filterable f, Ord c)
  => [c]
  -> [c]
  -> f (Event c m a)
  -> (f (Event c m a), f (Event c m a))
splitByConcepts c1 c2 es =
  (filterEvents (containsConcepts c1) es, filterEvents (containsConcepts c2) es)

{-|
Tally the number of events in a container satisfying the given predicate.
-}
tallyEvents
  :: (W.Witherable f) => Predicate (Event c m a) -> f (Event c m a) -> Int
tallyEvents p = length . filterEvents p
