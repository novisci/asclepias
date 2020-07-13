{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Hasklepias Interval Operations
Description : Functions for working with Intervals
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

module Hasklepias.IntervalOperations (

   combineIntervals
 , gaps

) where

import IntervalAlgebra
import Hasklepias.Types.Event
import Data.Semigroup
import Data.Maybe (catMaybes)

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

