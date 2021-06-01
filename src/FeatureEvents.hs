{-|
Module      : Functions for composing features from events  
Description : Functions for composing features. 
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

Provides functions used in defining 'Feature's.
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE Safe #-}

module FeatureEvents(

    -- * Container predicates
      isNotEmpty
    , atleastNofX
    , twoXOrOneY

    -- * Finding occurrences of concepts
    , nthConceptOccurrence
    , firstConceptOccurrence

    -- * Reshaping containers
    , allPairs
    , splitByConcepts

    -- * Create filters
    , makeConceptsFilter
    , makePairedFilter
) where

import Data.Text                            ( Text )
import Control.Applicative                  ( Applicative(liftA2) )
import IntervalAlgebra                      ( Intervallic(..)
                                            , ComparativePredicateOf1
                                            , ComparativePredicateOf2
                                            , Interval )
import IntervalAlgebra.PairedInterval       ( PairedInterval, getPairData )
-- import IntervalAlgebra.IntervalUtilities    ( compareIntervals )
import EventData                            ( Events
                                            , Event
                                            , ConceptEvent
                                            , ctxt )
import EventData.Context                    ( Concept
                                            , Concepts
                                            , Context
                                            , HasConcept( hasConcepts ) )
import Safe                                 ( headMay, lastMay ) 
import Data.Bool                       ( Bool, (&&), not, (||) )
import Data.Function                   ( (.), ($) )
import Data.Int                        ( Int )
import Data.List                       ( filter, length, null )
import Data.Maybe                      ( Maybe(..) )
import Data.Ord                        ( Ord((>=)) )


-- | Is the input list empty? 
isNotEmpty :: [a] -> Bool
isNotEmpty = not.null

-- | Filter 'Events' to those that have any of the provided concepts.
makeConceptsFilter ::
       [Text]    -- ^ the list of concepts by which to filter 
    -> Events a
    -> Events a
makeConceptsFilter cpts = filter (`hasConcepts` cpts)

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
firstConceptOccurrence = nthConceptOccurrence headMay

-- | Finds the *last* occurrence of an 'Event' with at least one of the concepts.
--   Assumes the input 'Events' list is appropriately sorted.
lastConceptOccurrence ::
      [Text]
    -> Events a
    -> Maybe (Event a)
lastConceptOccurrence = nthConceptOccurrence lastMay

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

-- | Takes a predicate of intervals and a predicate on the data part of a 
--   paired interval to create a single predicate such that both input
--   predicates should hold.
makePairPredicate ::  Ord a =>
       ComparativePredicateOf2 (i0 a) ((PairedInterval b) a)
    -> i0 a
    -> (b -> Bool)
    -> (PairedInterval b a -> Bool)
makePairPredicate pi i pd x =  pi i x && pd (getPairData x)

-- | 
makePairedFilter :: Ord a => 
       ComparativePredicateOf2 (i0 a) ((PairedInterval b) a)
    -> i0 a
    -> (b -> Bool)
    -> [PairedInterval b a]
    -> [PairedInterval b a]
makePairedFilter fi i fc = filter (makePairPredicate fi i fc)

-- | Generate all pair-wise combinations from two lists.
allPairs :: [a] -> [b] -> [(a, b)]
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