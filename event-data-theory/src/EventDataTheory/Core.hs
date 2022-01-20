{-|
Module      : Hasklepias Event Type
Description : Defines the Event type and its component types, constructors, 
              and class instance
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module EventDataTheory.Core
  ( Event
  , getEvent
  , getContext
  , Source(..)
  , Concept
  , Concepts
  , Context(..)
  , toConcepts
  , packConcept
  , unpackConcept
  , packConcepts
  , unpackConcepts
  , hasConcept
  , hasAnyConcepts
  , hasAllConcepts
  , liftToEventPredicate
  ) where

import           Data.Bool                      ( Bool )
import           Data.Eq                        ( Eq )
import           Data.Foldable                  ( all
                                                , any
                                                )
import           Data.Function                  ( ($)
                                                , (.)
                                                )
import           Data.Functor                   ( fmap )
import           Data.Functor.Contravariant     ( Contravariant(contramap)
                                                , Predicate(..)
                                                )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Monoid                    ( Monoid(..) )
import           Data.Ord                       ( Ord )
import           Data.Semigroup                 ( Semigroup(..) )
import           Data.Set                       ( Set
                                                , fromList
                                                , map
                                                , member
                                                , toList
                                                )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( Integer )
import           GHC.Show                       ( Show(..) )
import           IntervalAlgebra                ( Interval
                                                , Intervallic(..)
                                                , PairedInterval
                                                , getPairData
                                                , makePairedInterval
                                                )

import           Witch                          ( From(..)
                                                , into
                                                , via
                                                )

{- |
The 'Event' type puts a certain amount of structure on
temporally organized data, 
while being flexible in the details.
An 'Event d c a' contains information about
when something occurred (the 'Interval a')
and what occurred (the 'Context d c').
The type parameters `d', 'c', and 'a' allow to specify 
the types for the 'Context''s @d@omain and @c@oncepts
and for the type of the 'Interval' end points.

The 'Event' type parameters are ordered from changing the least often to most often.
A @d@omain tends to be shared across projects.
For example, multiple projects use data from insurance claims, 
and thus share a single domain. 
A project often defines its own @c@oncepts, 
though concepts can be shared across projects.
Within a project, multiple 'Interval' types may used.
Data may be imported as 'Interval Day', 
but then modified to 'Interval Integer' based on some reference point.

The contents of a 'Context' are explained in a separate section,
but we give a couple examples of using events here.

The 'event' function is a smart constructor for 'Event'.

>>> :set -XOverloadedStrings
>>> import IntervalAlgebra ( beginerval ) 

>>> data SomeDomain = A | B deriving (Eq, Ord, Show, Generic)
>>> type MyEvent = Event SomeDomain T.Text Integer
>>> let myEvent = event (beginerval 5 0) (Context (packConcepts ["foo"]) A Nothing) :: MyEvent
>>> show myEvent
"MkEvent {(0, 5), Context {concepts = Concepts (fromList [Concept \"foo\"]), facts = A, source = Nothing}}"

>>> hasAnyConcepts myEvent (["foo", "duck"] :: [T.Text])
True

>>> hasAllConcepts myEvent (["foo", "duck"] :: [T.Text])
False

>>> data NewDomain = A T.Text | B Integer deriving (Eq, Ord, Show, Generic)
>>> data MyConcepts = Foo | Bar | Baz deriving (Eq, Ord, Show, Generic) 
>>> type NewEvent = Event NewDomain MyConcepts Integer
>>> let newEvent = event (beginerval 5 0) (Context (packConcepts [Foo, Bar]) (A "cool") Nothing) :: NewEvent
>>> show newEvent
"MkEvent {(0, 5), Context {concepts = Concepts (fromList [Concept Foo,Concept Bar]), facts = A \"cool\", source = Nothing}}"

>>> hasConcept newEvent Foo
True

>>> hasConcept newEvent Baz
False


-}

newtype Event d c a = MkEvent ( PairedInterval (Context d c) a )
  deriving (Eq, Show, Generic)

instance (Ord a) => Intervallic (Event d c) a where
  getInterval (MkEvent x) = getInterval x
  setInterval (MkEvent x) y = MkEvent $ setInterval x y

instance Ord c => HasConcept (Event d c a) c where
  hasConcept e = hasConcept (getContext e)

-- | A smart constructor for 'Event d c a's.
event
  :: (Show d, Eq d, Generic d, Show c, Eq c, Ord c, Generic c)
  => Interval a
  -> Context d c
  -> Event d c a
event i c = MkEvent (makePairedInterval c i)

-- | Unpack an 'Event' from its constructor.
getEvent :: Event d c a -> PairedInterval (Context d c) a
getEvent (MkEvent x) = x

-- | Get the 'Context' of an 'Event'. 
getContext :: Event d c a -> Context d c
getContext = getPairData . getEvent


{- |
A 'Context' contains information about what ocurred during an 'Event''s interval.
This information is carried in context's @concepts@ and/or @facts@.
'Concepts' are set of tags that can be used to identify and filter events
using the 'hasConcept' function
or the related 'hasAnyConcepts' and 'hasAllConcepts' functions.
The @facts@ field contains data of type @d@. 
The @d@ stands for @d@omain, 
meaning the scope and shape of facts
relevant to a particular scientific line of work.
For example, some studies using health care claims data may be sufficiently different
in scope, semanitcs, and aims to warrant having a different collection of facts
from, say, electronic medical records data. 
However, one could create a collection of facts that includes both claims and EHR data.
By having a 'Context' parametrized by the shape of a domain's facts,
users are free to define the structure of their facts as needed. 

A context also has a @source@ field
which may be used to carry information about the provenance of the data.

-}
data Context d c = Context
  { concepts :: Concepts c
  , facts    :: d
  , source   :: Maybe Source
  }
  deriving (Eq, Show, Generic)

instance Ord c => HasConcept (Context d c) c where
  hasConcept c = hasConcept (concepts c)

{- |
-}
data Source = Source
  { column   :: Maybe T.Text
  , file     :: Maybe T.Text
  , row      :: Maybe Integer
  , table    :: T.Text
  , database :: T.Text
  }
  deriving (Eq, Show, Generic)

-- | A @Concept@ is simply a tag or label for an 'Event'.
newtype Concept c = Concept c deriving (Eq, Ord, Show, Generic)

instance From (Concept c) c where
instance From c (Concept c) where

-- | Wrap value as a Concept
packConcept :: c -> Concept c
packConcept = into

-- | Unwrap a value from a Concept
unpackConcept :: Concept c -> c
unpackConcept = into

{- |  
@Concepts c@ is a 'Set' of 'Concept c's.
Concepts inherit the monoidal properties of 'Set', by 'Data.Set.union'.
-}
newtype Concepts c = Concepts ( Set ( Concept c ) )
    deriving (Eq, Show, Generic)

instance (Ord c) => Semigroup ( Concepts c ) where
  Concepts x <> Concepts y = Concepts (x <> y)

instance (Ord c) => Monoid ( Concepts c ) where
  mempty = Concepts mempty

instance (Ord c) => From (Concepts c) (Set (Concept c)) where
instance (Ord c) => From (Set (Concept c)) (Concepts c) where

instance (Ord c) => From (Set (Concept c)) [c] where
  from x = from @(Set c) (Data.Set.map (from @(Concept c)) x)

instance (Ord c) => From [c] (Set (Concept c)) where
  from x = from @[Concept c] (fmap from x)

instance (Ord c) => From (Concepts c) [c] where
  from = via @(Set (Concept c))

instance (Ord c) => From [c] (Concepts c) where
  from = via @(Set (Concept c))

-- | Put a list of values into a set of concepts.
packConcepts :: Ord c => [c] -> Concepts c
packConcepts = from

-- | Take a set of concepts to a list of values.
unpackConcepts :: (Ord c) => Concepts c -> [c]
unpackConcepts = from

-- | Constructor for 'Concepts'.
toConcepts :: (Ord c) => Set (Concept c) -> Concepts c
toConcepts = from

{- |
The 'HasConcept' typeclass provides predicate functions
for determining whether an @a@ contains a concept.
-}
class HasConcept a c where
    hasConcept  :: a -> c -> Bool

instance (Ord c) => HasConcept (Concepts c) c where
  hasConcept (Concepts e) concept = member (Concept concept) e

-- | Does an @a@ have *any* of a list of 'Concept's?
hasAnyConcepts :: HasConcept a c => a -> [c] -> Bool
hasAnyConcepts x = any (\c -> x `hasConcept` c)

-- | Does an @a@ have *all* of a list of `Concept's?
hasAllConcepts :: HasConcept a c => a -> [c] -> Bool
hasAllConcepts x = all (\c -> x `hasConcept` c)

{- |
Provides a common interface to lift a 'Predicate' on some component
of an 'Event' to a 'Predicate (Event d c a)'.
For example, if @x@ is a 'Predicate' on some 'Context d c',
@liftToEventPredicate x@ yields a @Predicate (Event d c a)@, 
thus the predicate then also be applied to @Event@s.
-}
class EventPredicate element d c a where
  liftToEventPredicate :: Predicate element -> Predicate (Event d c a)

instance EventPredicate (Context d c) d c a where
  liftToEventPredicate = contramap getContext

instance EventPredicate d d c a where
  liftToEventPredicate = contramap (facts . getContext)

instance EventPredicate (Concepts c) d c a where
  liftToEventPredicate = contramap (concepts . getContext)

instance EventPredicate (Maybe Source) d c a where
  liftToEventPredicate = contramap (source . getContext)

instance (Ord a) => EventPredicate (Interval a) d c a where
  liftToEventPredicate = contramap getInterval



