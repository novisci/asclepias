{-|
Module      : Functions for composing features from events  
Description : Functions for composing features. 
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

Provides functions used in defining @'FeatureCompose.Feature'@ from 
@'EventData.Event'@s.
-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FeatureEvents(
    -- ** Container predicates
      isNotEmpty
    , atleastNofX
    , anyGapsWithinAtLeastDuration
    , allGapsWithinLessThanDuration

    -- **  Finding occurrences of concepts
    , nthConceptOccurrence
    , firstConceptOccurrence

    -- ** Reshaping containers
    , allPairs
    , splitByConcepts

    -- ** Create filters
    , makeConceptsFilter
    , makePairedFilter

    -- ** Functions for working with Event Domains
    , viewBirthYears
    , previewBirthYear

    -- ** Function for manipulating intervals
    , lookback
    , lookahead

    -- ** Misc functions
    , computeAgeAt
) where


import IntervalAlgebra                      ( Intervallic(..)
                                            , IntervalSizeable(..)
                                            , ComparativePredicateOf1
                                            , ComparativePredicateOf2
                                            , Interval
                                            , IntervalCombinable
                                            , beginerval
                                            , enderval )
import IntervalAlgebra.PairedInterval       ( PairedInterval, getPairData )
import IntervalAlgebra.IntervalUtilities    ( durations, gapsWithin, gaps' )
import EventData                            ( Events
                                            , Event
                                            , ConceptEvent
                                            , ctxt )
import EventData.Context                    ( Concept
                                            , Concepts
                                            , Context
                                            , HasConcept( hasConcepts )
                                            , facts )
import EventData.Context.Domain             ( Domain
                                            , demo
                                            , info
                                            , _Demographics )
import Safe                                 ( headMay, lastMay )
import Control.Applicative                  ( Applicative(liftA2) )
import Control.Monad                        ( Functor(fmap), (=<<) )
import Control.Lens                         ( preview, (^.) )
import Data.Bool                            ( Bool(..), (&&), not, (||) )
import Data.Either                          ( either )
import Data.Eq                              ( Eq )
import Data.Foldable                        ( Foldable(length, null), all, any )
import Data.Function                        ( (.), ($), const )
import Data.Functor                         ( Functor(fmap) )
import Data.Int                             ( Int )
import Data.Maybe                           ( Maybe(..), maybe, mapMaybe )
import Data.Monoid                          ( Monoid )
import Data.Ord                             ( Ord(..) )
import Data.Time.Calendar                   ( Day, Year, diffDays )
import Data.Text                            ( Text )
import Data.Text.Read                       ( rational )
import Data.Tuple                           ( fst )
import Witherable                           ( filter, Filterable )
import GHC.Num                              ( Integer, fromInteger )
import GHC.Real                             ( RealFrac(floor), (/) )

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


-- | Create a predicate function that checks whether within a provided spanning
--   interval, are there (e.g. any, all) gaps of (e.g. <, <=, >=, >) a specified
--   duration among  the input intervals?
makeGapsWithinPredicate ::
       ( Foldable t
       , Monoid (t (Interval a))
       , Applicative t
       , Filterable t
       , IntervalSizeable a b
       , IntervalCombinable i0 a
       , IntervalCombinable i1 a) =>
          ((b -> Bool) ->  t b -> Bool)
        -> (b -> b -> Bool)
        -> (b -> i0 a -> t (i1 a) -> Bool)
makeGapsWithinPredicate f op gapDuration interval l =
     maybe False (f (`op` gapDuration) . durations) (gapsWithin interval l)

-- | Within a provided spanning interval, are there any gaps of at least the
--   specified duration among the input intervals?
anyGapsWithinAtLeastDuration ::
      (IntervalSizeable a b, IntervalCombinable i0 a, IntervalCombinable i1 a) =>
        b       -- ^ duration of gap
        -> i0 a  -- ^ within this interval
        -> [i1 a]
        -> Bool
anyGapsWithinAtLeastDuration = makeGapsWithinPredicate any (>=)

-- | Within a provided spanning interval, are all gaps less than the specified
--   duration among the input intervals?
--
-- >>> allGapsWithinLessThanDuration 30 (beginerval 100 (0::Int)) [beginerval 5 (-1), beginerval 99 10]
-- True
allGapsWithinLessThanDuration ::
      (IntervalSizeable a b, IntervalCombinable i0 a, IntervalCombinable i1 a) =>
        b       -- ^ duration of gap
        -> i0 a  -- ^ within this interval
        -> [i1 a]
        -> Bool
allGapsWithinLessThanDuration = makeGapsWithinPredicate all (<)

-- | Utility for reading text into a maybe integer
intMayMap :: Text -> Maybe Integer -- TODO: this is ridiculous
intMayMap x = fmap floor (either (const Nothing) (Just . fst) (Data.Text.Read.rational x))

-- | Preview birth year from a domain
previewBirthYear :: Domain -> Maybe Year
previewBirthYear dmn = intMayMap =<< ((^.demo.info) =<< preview _Demographics dmn)

-- | Returns a (possibly emtpy) list of birth years from a set of events
viewBirthYears :: Events a -> [Year]
viewBirthYears = mapMaybe (\e -> previewBirthYear =<< Just (ctxt e^.facts ))

-- | Compute the "age" in years between two calendar days. The difference between
--   the days is rounded down.
computeAgeAt :: Day -> Day -> Integer
computeAgeAt bd at = floor (fromInteger (diffDays at bd) / 365.25)

-- | Creates a new @Interval@ of a provided lookback duration ending at the 
--   'begin' of the input interval.
--
-- >>> lookback 4 (beginerval 10 (1 :: Int))
-- (-3, 1)
lookback :: (Intervallic i a, IntervalSizeable a b) =>
    b   -- ^ lookback duration
    -> i a
    -> Interval a
lookback d x = enderval d (begin x)

-- | Creates a new @Interval@ of a provided lookahead duration beginning at the 
--   'end' of the input interval.
--
-- >>> lookahead 4 (beginerval 1 (1 :: Int))
-- (2, 6)
lookahead :: (Intervallic i a, IntervalSizeable a b) =>
    b   -- ^ lookahead duration
    -> i a
    -> Interval a
lookahead d x = beginerval d (end x)

