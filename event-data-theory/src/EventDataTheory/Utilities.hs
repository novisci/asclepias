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
  ) where


import           Data.Functor.Contravariant     ( Predicate(..) )
import           Data.Semigroup
import           EventDataTheory.Core

-- | Combine two 'Data.nctor.Contravariant.Predicate's by "or".
(|||) :: Predicate a -> Predicate a -> Predicate a
(|||) f g =
  Predicate (\x -> getAny (Any (getPredicate f x) <> Any (getPredicate g x)))

-- | Combine two 'Data.nctor.Contravariant.Predicate's by "and".
(&&&) :: Predicate a -> Predicate a -> Predicate a
(&&&) f g =
  Predicate (\x -> getAll (All (getPredicate f x) <> All (getPredicate g x)))

{-| 
Creates a predicate to check that an 'Event' contains
any of a given set of concepts.
-}
containsConcepts :: (Ord c) => [c] -> Predicate (Event d c a)
containsConcepts cpt = Predicate (`hasAnyConcepts` cpt)
