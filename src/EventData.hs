{-|
Module      : Hasklepias Event Type
Description : Defines the Event type and its component types, constructors, 
              and class instance
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

module EventData(
   Event
 , Events
 , ConceptEvent
 , event
 , ctxt
 , toConceptEvent
 , toConceptEventOf
 , mkConceptEvent
) where

import GHC.Show                         ( Show(show) )
import Data.Function                    ( ($) )
import Data.Set                         ( member, fromList, intersection )
import Data.Ord                         ( Ord )
import IntervalAlgebra                  ( Interval
                                        , Intervallic
                                        , Intervallic (getInterval) )
import IntervalAlgebra.PairedInterval   ( PairedInterval
                                        , makePairedInterval
                                        , getPairData )
import EventData.Context                ( HasConcept(..)
                                        , Concepts
                                        , Concept
                                        , packConcept
                                        , Context (getConcepts)
                                        , fromConcepts
                                        , toConcepts )


-- | An Event @a@ is simply a pair @(Interval a, Context)@.
type Event a = PairedInterval Context a

-- instance (Ord a, Show a) => Show (Event a) where
--   show x = "{" ++ show (getInterval x) ++ ", " ++ show (ctxt x) ++ "}"

instance HasConcept (Event a) where
    hasConcept x y = ctxt x `hasConcept` y

-- | A smart constructor for 'Event a's.
event :: Interval a -> Context -> Event a
event i c = makePairedInterval c i

-- | Access the 'Context' of an 'Event a'.
ctxt :: Event a -> Context
ctxt = getPairData

-- | An event containing only concepts and an interval
type ConceptEvent a = PairedInterval Concepts a

-- instance (Ord a, Show a) => Show (ConceptEvent a) where
--   show x = "{" ++ show (getInterval x) ++ ", " ++ show (getPairData x) ++ "}"

instance HasConcept (ConceptEvent a) where
    hasConcept e concept = member (packConcept concept) (fromConcepts $ getPairData e)

-- | Drops an @Event@ to a @ConceptEvent@ by moving the concepts in the data
--   position in the paired interval and throwing out the facts and source.
toConceptEvent :: (Show a, Ord a) => Event a -> ConceptEvent a
toConceptEvent e = makePairedInterval (getConcepts $ ctxt e) (getInterval e)

toConceptEventOf :: (Show a, Ord a) => [Concept] -> Event a -> ConceptEvent a
toConceptEventOf cpts e =
    makePairedInterval
        (toConcepts $ intersection (fromList cpts) (fromConcepts $ getConcepts $ ctxt e))
        (getInterval e)

-- |
mkConceptEvent :: (Show a, Ord a) => Interval a -> Concepts -> ConceptEvent a
mkConceptEvent i c = makePairedInterval c i

-- | A @List@ of @Event a@
-- 
-- NOTE (20190911): I (B. Saul) am starting out the Events type as a 
-- list of the Event type. This may be not be the optimal approach,
-- especially with regards to lookup/filtering the list. Ideally,
-- we could do one pass through the ordered container (whatever it is)
-- to identify events by concept; rather than repeated evaluations of
-- the lookup predicates. This could be handled by, for example, 
-- representing Events has a Map with a list of concept indices. 
-- But this gets us off the ground.
type Events a = [Event a]
