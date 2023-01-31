{-|
Module      : Cohort IndexSet
Copyright   : (c) TargetRWE 2023
License     : BSD3
Maintainer  : bbrown@targetrwe.com
              ljackman@targetrwe.com
              dpritchard@targetrwe.com
-}

{-# LANGUAGE TypeFamilies #-}
module Cohort.IndexSet
  ( IndexSet
    , Cohort.IndexSet.null
    , Cohort.IndexSet.foldl'
  , fromList
  , toList
  ) where

import qualified Data.Set        as Set (Set, foldl', null)
import           EventDataTheory (Interval)
import           GHC.Exts        (IsList (..))

{-|
A type containing a @Data.Set.'Set'@ of type @Interval b@, values of which
serve as index times when defining cohorts.  In cohort terminology, indices are
the points in time at which an observational unit
can be assessed whether it meets the criteria for inclusion in the cohort.  For
example, when @i@ is @Interval Day@, then index events are days.

A reason for using a set as the underlying type is that indices must be unique
within a subject. A subject cannot have multiple observational units for a
given index.

Construct an @IndexSet@ using @toList@.
-}
newtype IndexSet b
  = MkIndexSet (Set.Set (Interval b))
  deriving (Eq, Show)

instance (Ord b) => IsList (IndexSet b) where
  type Item (IndexSet b) = (Interval b)
  toList (MkIndexSet s) = toList s
  fromList idxs = MkIndexSet $ fromList idxs

-- | Specialized @Set.'null'@.
null :: IndexSet b -> Bool
null (MkIndexSet idxs) = Set.null idxs

-- | Specialized @Set.'foldl''@.
foldl' :: (a -> Interval b -> a) -> a -> IndexSet b -> a
foldl' f x0 (MkIndexSet s) = Set.foldl' f x0 s
