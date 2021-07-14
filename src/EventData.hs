{-|
Module      : Hasklepias Event Type
Description : Defines the Event type and its component types, constructors, 
              and class instance
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE Safe #-}

module EventData(

 -- * Events
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
                                        , Context(..)
                                        , getConcepts
                                        , toConcepts )


-- | An Event @a@ is simply a pair @(Interval a, Context)@.
type Event a = PairedInterval Context a

instance HasConcept (Event a) where
    hasConcept x y = ctxt x `hasConcept` y

-- | A smart constructor for 'Event a's.
event :: Interval a -> Context -> Event a
event i c = makePairedInterval c i

-- | Get the 'Context' of an 'Event a'.
ctxt :: Event a -> Context
ctxt = getPairData

-- | An event containing only concepts and an interval
type ConceptEvent a = PairedInterval Concepts a

instance HasConcept (ConceptEvent a) where
    hasConcept e concept = member (packConcept concept) (getConcepts $ getPairData e)

-- | Drops an @Event@ to a @ConceptEvent@ by moving the concepts in the data
--   position in the paired interval and throwing out the facts and source.
toConceptEvent :: (Show a, Ord a) => Event a -> ConceptEvent a
toConceptEvent e = makePairedInterval (_concepts $ ctxt e) (getInterval e)

-- | Creates a new @'ConceptEvent'@ from an @'Event'@ by taking the intersection
-- of the list of Concepts in the first argument and any Concepts in the @'Event'@.
-- This is a way to keep only the concepts you want in an event.
toConceptEventOf :: (Show a, Ord a) => [Concept] -> Event a -> ConceptEvent a
toConceptEventOf cpts e =
    makePairedInterval
        (toConcepts $ intersection (fromList cpts) (getConcepts $ _concepts $ ctxt e))
        (getInterval e)

-- | Create a new @'ConceptEvent'@.
mkConceptEvent :: (Show a, Ord a) => Interval a -> Concepts -> ConceptEvent a
mkConceptEvent i c = makePairedInterval c i

-- | A @List@ of @Event a@
type Events a = [Event a]
