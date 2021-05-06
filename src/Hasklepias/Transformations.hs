{-|
Module      : Hasklepias event transformation functions 
Description : Functions for composing features. 
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Hasklepias.Transformations(
    stateConfigurator
  , StateEvent
  , State
  , meetEvents
  , mergeStateEvents
  , transformToMeetingSequence
) where

import Data.List ( map, foldl' )
import IntervalAlgebra
import IntervalAlgebra.PairedInterval
    ( mkPairedInterval, pairData, PairedInterval )
import Hasklepias.Types ( Concept, Concepts, ConceptEvent )
import Data.Set(map, toList, lookupIndex, fromList, member)
import Data.Maybe ( maybeToList )
import Safe (tailSafe, initSafe, headMay)


{- | 
-}
transformToMeetingSequence :: 
                (IntervalAlgebraic (PairedInterval Concepts) a
                 , IntervalAlgebraic (PairedInterval State) a
                 , IntervalSizeable a b)=>
        ValidTStates
     -> [ConceptEvent a]
     -> [ConceptEvent a]
transformToMeetingSequence vs =   sEventsTocEvents vs 
                                . meetEvents
                                . cEventsToSEvents vs


{- 
A 'State' is a simply list of 'Bool's, wherein each element is specified to 
correspond to a particular state.
-}
type State        = [Bool]
type ValidTStates = [Concept]
type StateEvent a = PairedInterval State a

instance (Ord a, Show a) => Show (StateEvent a) where
  show x = "{" ++ show (getInterval x) ++ ", " ++ show (pairData x) ++ "}"

-- | Create a 'State' of length 'i' where all elements are 'False'.
none :: Int -> State
none i = replicate i False

{- | 
Creates a @State@ from a @ValidTState@ and an @InTState@. If the 
@InTState@ is in @ValidTState@, a @State@ corresponding to that input
is returned; otherwise, @none@ is returned.
-}

stateConfigurator :: ValidTStates -> Concepts -> State
stateConfigurator x cs = Data.List.map (`member` cs) x

-- | Convert a @State@ back to a @OutTState
-- stateToText :: ValidTStates -> State -> OutTState
-- stateToText l s = Data.List.map snd $ filter fst (zip s l)

stateToConcepts :: ValidTStates -> State -> Concepts
stateToConcepts vs s = fromList (Data.List.map snd $ filter fst (zip s vs))

-- | Combine two States to create a new State
combineStates :: State -> State -> State
combineStates = zipWith (||)

convertConceptToStateEvent :: (IntervalAlgebraic (PairedInterval Concepts) a
                             , IntervalAlgebraic (PairedInterval State) a) =>
        ValidTStates
     -> ConceptEvent a
     -> StateEvent a
convertConceptToStateEvent vs x =
    mkPairedInterval (stateConfigurator vs (pairData x)) (getInterval x)

convertStateToConceptEvent :: (IntervalAlgebraic (PairedInterval Concepts) a
                             , IntervalAlgebraic (PairedInterval State) a) =>
        ValidTStates
     -> StateEvent a
     -> ConceptEvent a
convertStateToConceptEvent vs x =
    mkPairedInterval (stateToConcepts vs (pairData x)) (getInterval x)

cEventsToSEvents :: (IntervalAlgebraic (PairedInterval Concepts) a
                   , IntervalAlgebraic (PairedInterval State) a) =>
        ValidTStates
     -> [ConceptEvent a]
     -> [StateEvent a]
cEventsToSEvents vs = Data.List.map (convertConceptToStateEvent vs)

sEventsTocEvents :: (IntervalAlgebraic (PairedInterval Concepts) a
                   , IntervalAlgebraic (PairedInterval State) a) =>
        ValidTStates
     -> [StateEvent a]
     -> [ConceptEvent a]
sEventsTocEvents vs = Data.List.map (convertStateToConceptEvent vs)


{- | 
Convert an ordered sequence of events that may have any interval
relation (before, starts, etc) into a sequence of sequentially 
meeting events. That is, a sequence where one the end of one event
meets the beginning of the subsequent event. The states of the input
events are combined using the combineStates function. Internally this 
uses the function h. 
-}
meetEvents :: (IntervalAlgebraic (PairedInterval State) a
              , IntervalSizeable a b) =>
        [StateEvent a] -> [StateEvent a]
meetEvents x = mtEvt ([], []) (mtEvt ([], []) x)

{- | 
The internal function for converting a non-disjoint, ordered sequence of
events into a disjoint, ordered sequence of events. The function operates
by recursion on a pair of events and the input events. The first of the 
is the accumulator set -- the disjoint events that need no longer be 
compared to input events. The second of the pair are disjoint events that
still need to be compared to be input events. 
-}
mtEvt :: (IntervalAlgebraic (PairedInterval State) a
         , IntervalSizeable a b) =>
            ([StateEvent a], [StateEvent a])
         -> [StateEvent a]
         -> [StateEvent a]
mtEvt (acc, o:os) []     = acc ++ o:os           -- the "final" pattern
mtEvt (acc, [])   []     = acc                 -- another "final" pattern 
mtEvt (acc, [])   (e:es) = mtEvt (acc, [e]) es -- the "initialize" pattern
mtEvt (acc, o:os) (e:es)                       -- the "operating" patterns 
     -- If input event is equal to the first comparator, skip the comparison.
    | e == o    = mtEvt (acc, o:os) es

     {- If the period of o is either before or meets the period of e, then 
     the first of the combined events can be put into the accumulator. 
     That is, since the inputs events are ordered, once the beginning of o 
     is before or meets e, then we are assured that all periods up to the 
     beginning of o are fully disjoint and subsequent input events will 
     not overlap these in any way. -}
    | (before <|> meets) o e = mtEvt (acc ++ nh, mtEvt ([], nt) os ) es

    --The standard recursive operation.
    | otherwise = mtEvt (acc,  mtEvt ([], n) os ) es
  where n  = mergeStateEvents o e
        nh = maybeToList (headMay n)
        nt = tailSafe n

meetEvents2 :: (IntervalAlgebraic (PairedInterval State) a) =>
    StateEvent a -> StateEvent a -> [StateEvent a]
meetEvents2 x y =
    if pairData x == pairData y then
        [ mkPairedInterval s (extenterval x y) ]
    else [x, y]
    where s = combineStates (pairData x) (pairData y)

newtype Box a = Box { unBox :: [a] }
instance (IntervalAlgebraic (PairedInterval State) a) =>
           Semigroup (Box (StateEvent a)) where
    Box x <> Box y
       | null x         = Box y
       | null y         = Box x
       | otherwise      = Box $ initSafe x ++ meetEvents2 lx fy ++ tailSafe y
       where lx = last x
             fy = head y

foldMeets :: (IntervalAlgebraic (PairedInterval State) a) =>
        [StateEvent a] -> [StateEvent a]
foldMeets l = unBox $ foldl' (<>) (Box []) (Data.List.map (\z -> Box [z]) l)

{- | 
Takes two *ordered* events, x <= y, and "disjoins" them in the case that the
two events have different states, creating a sequence (list) of new events that 
sequentially meet one another. Since x <= y, there are 7 possible interval
relations between x and y. If the states of x and y are equal and x is not 
before y, then x and y are combined into a single event. 
-}
mergeStateEvents :: (IntervalAlgebraic (PairedInterval State) a
                  , IntervalSizeable a b) =>
                     StateEvent a
                  -> StateEvent a
                  -> [StateEvent a]
mergeStateEvents o e
   | x `before` y      = [ x, evp e1 b2 (none ls), y ]
   | x `meets` y       = foldMeets [ x, y ]
   | x `overlaps` y    = foldMeets [ evp b1 b2 s1, evp b2 e1 sc, evp e1 e2 s2 ]
   | x `finishedBy` y  = foldMeets [ evp b1 b2 s1, ev i2 sc ]
   | x `contains` y    = foldMeets [ evp b1 b2 s1, evp b2 e2 sc, evp e2 e1 s1 ]
   | x `starts` y      = foldMeets [ ev i1 sc, evp e1 e2 s2 ]
   | x `equals` y      = [ ev i1 sc ]
   where x  = min o e
         y  = max o e
         i1 = getInterval x
         i2 = getInterval y
         s1 = pairData x
         s2 = pairData y
         ls = length s1
         sc = combineStates s1 s2
         b1 = begin x
         b2 = begin y
         e1 = end x
         e2 = end y
         ev = flip mkPairedInterval
         evp = \b e s -> ev (beginerval (diff e b) b) s