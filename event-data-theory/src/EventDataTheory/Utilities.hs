{-|
Module      : Misc utilities useful in working with events 
-}

module EventDataTheory.Utilities
  ( (|||)
  , (&&&)
  , Predicate(..)
  , containsConcepts
  ) where


import           Data.Functor.Contravariant     ( Predicate(..) )
import           Data.Semigroup
import           EventDataTheory.Core

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
containsConcepts :: (Ord c) => [c] -> Predicate (Event d c a)
containsConcepts cpt = Predicate (`hasAnyConcepts` cpt)
