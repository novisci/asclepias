{-|
Module      : Cohort IndexSet
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cohort.IndexSet
  ( IndexSet
  , makeIndexSet
  ) where

import qualified Data.List.NonEmpty as NEL (nonEmpty, toList)
import qualified Data.Set.NonEmpty  as Set (NESet, fromList, singleton, toList)
import           GHC.Generics       (Generic)
import           Witch              (From (..), into)

{-|
A type containing (maybe) a @Data.Set.NonEmpty.NESet@ (nonempty) of type @i@,
values of which serve as indices when defining cohorts.
In cohort terminology,
indices are the points in time at which an observational unit
can be assessed whether it meets the criteria for inclusion in the cohort.
For example, when @i@ is @Interval Day@,
then index events are days.

A reason for using @Data.Set.NonEmpty.NESet@ as the underlying type
is that indices must be unique within a subject.
a subject cannot have multiple observational units for a given index.

Use the 'makeIndexSet' function for creating an @IndexSet@.
-}
newtype IndexSet i = MkIndexSet ( Maybe (Set.NESet i) )
  deriving (Eq, Show, Generic)

instance From (IndexSet i) (Maybe (Set.NESet i)) where
instance From (IndexSet i) (Maybe [i]) where
  from (MkIndexSet x) = fmap (NEL.toList . Set.toList) x

instance From (Maybe i) (IndexSet i) where
  from x = case x of
    Nothing -> MkIndexSet Nothing
    Just i  -> MkIndexSet $ Just (Set.singleton i)
instance From i (IndexSet i) where
  from x = MkIndexSet $ Just (Set.singleton x)
instance (Ord i) => From [i] (IndexSet i) where
  from x = MkIndexSet $ fmap Set.fromList (NEL.nonEmpty x)

{-|
Smart Constructor for creating an `IndexSet` from a list of indices.
-}
makeIndexSet :: (Ord i) => [i] -> IndexSet i
makeIndexSet = into

