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
  , containsTag
  , findOccurrenceOfEvent
  , firstOccurrenceOfTag
  , lastOccurrenceOfTag
  , splitByTags
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
any of a given list of tags.
-}
containsTag :: (Ord t) => [t] -> Predicate (Event t m a)
containsTag tList = Predicate (`hasAnyTag` tList)

{-|
Filter a container of events by a predicate.
-}
filterEvents
  :: (W.Filterable f)
  => Predicate (Event t m a)
  -> f (Event t m a)
  -> f (Event t m a)
filterEvents p = W.filter (getPredicate p)

{-|
Filter a container of 'Event's 
to a single @'Maybe' 'Event'@,
based on a provided function,
with the provided tag set. 

For example,
see 'firstOccurrenceOfTag' and
'lastOccurrenceOfTag'.
-}
findOccurrenceOfEvent
  :: (W.Filterable f)
  => (f (Event t m a) -> Maybe (Event t m a)) -- ^ function used to select a single event after the container is filtered
  -> Predicate (Event t m a) -- ^ predicate by which to filter
  -> f (Event t m a) -- ^ a container of events
  -> Maybe (Event t m a)
findOccurrenceOfEvent f p = f . filterEvents p

{-|
Finds the *first* occurrence of an 'Event' with at least one of the tags
if one exists.
Assumes the input events are appropriately sorted.
-}
firstOccurrenceOfTag
  :: (W.Witherable f, Ord t) => [t] -> f (Event t m a) -> Maybe (Event t m a)
firstOccurrenceOfTag x =
  findOccurrenceOfEvent (headMay . toList) (containsTag x)

{-|
Finds the *last* occurrence of an 'Event' with at least one of the tags
if one exists.
Assumes the input events list are appropriately sorted.
-}
lastOccurrenceOfTag
  :: (W.Witherable f, Ord t) => [t] -> f (Event t m a) -> Maybe (Event t m a)
lastOccurrenceOfTag x =
  findOccurrenceOfEvent (lastMay . toList) (containsTag x)

{-|
Split a container of @'Event'@s 
into a pair of @'Event'@s. 
The first element contains
events have any of the tags in the first argument,
similarly for the second element.

Note that one or both of the resulting containers
may be empty.
-}
splitByTags
  :: (W.Filterable f, Ord t)
  => [t]
  -> [t]
  -> f (Event t m a)
  -> (f (Event t m a), f (Event t m a))
splitByTags t1 t2 es =
  (filterEvents (containsTag t1) es, filterEvents (containsTag t2) es)

{-|
Tally the number of events in a container satisfying the given predicate.
-}
tallyEvents
  :: (W.Witherable f) => Predicate (Event t m a) -> f (Event t m a) -> Int
tallyEvents p = length . filterEvents p
