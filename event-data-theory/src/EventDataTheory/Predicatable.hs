{- |
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module EventDataTheory.Predicatable
  ( Predicatable(..)
  ) where

import           Data.Bool                      ( (&&)
                                                , Bool
                                                , (||)
                                                )
import           Data.Functor.Contravariant     ( Contravariant(contramap)
                                                , Predicate(..)
                                                )
{- |
Provides methods for composing predicate functions (i.e. @a -> Bool@) or 
'Predicate's by conjunction or disjunction.
-}
class Predicatable a where
  (|||) :: a -> a -> a
  (&&&) :: a -> a -> a

instance Predicatable (a -> Bool) where
  (|||) f g x = f x || g x
  (&&&) f g x = f x && g x

instance Predicatable (Predicate a) where
  (|||) p1 p2 = Predicate (getPredicate p1 ||| getPredicate p2)
  (&&&) p1 p2 = Predicate (getPredicate p1 &&& getPredicate p2)


