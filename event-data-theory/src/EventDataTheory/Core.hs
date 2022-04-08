{-|
Module      : Hasklepias Event Type
Description : Defines the Event type and its component types, constructors, 
              and class instance
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

NOTE: The types herein are how events are represently internally. 
Events may be represented in different structures for transferring or storing data, for example.
The To/FromJSON instances for types defined in this module are derived generically.
These can be useful for writing tests, for example, but
they are not designed to encode/decode data in the new line delimited format
defined in the 
[event data model docs](https://docs.novisci.com/edm-sandbox/latest/index.html#_event_representation)
See the neighboring EventLine module for types and To/FromJSON instances
designed for the purpose of marshaling data from JSON lines.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ConstraintKinds #-}

module EventDataTheory.Core
  ( Event
  , event
  , getEvent
  , getContext
  , Source(..)
  , Concept
  , Concepts
  , Context
  , ConceptsInterval
  , getFacts
  , getSource
  , getConcepts
  , context
  , toConcepts
  , packConcept
  , unpackConcept
  , packConcepts
  , unpackConcepts
  , hasConcept
  , hasAnyConcepts
  , hasAllConcepts
  , addConcepts
  , liftToEventPredicate
  , liftToEventFunction
  , liftToContextFunction
  , trimapEvent
  , bimapContext
  , mapConcepts
  , dropSource
  , SubjectID
  -- the following names are exported for haddock linking
  , HasConcept
  , EventPredicate
  , Eventable
  , FromJSONEvent
  , ToJSONEvent
  ) where

import           Control.DeepSeq                ( NFData )
import           Control.Monad                  ( liftM2
                                                , liftM3
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , Value(Number, String)
                                                )
import           Data.Binary                    ( Binary )
import           Data.Functor.Contravariant     ( Contravariant(contramap)
                                                , Predicate(..)
                                                )
import           Data.Set                       ( Set
                                                , fromList
                                                , map
                                                , member
                                                , toList
                                                )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           IntervalAlgebra                ( Interval
                                                , Intervallic(..)
                                                , PairedInterval
                                                , getPairData
                                                , makePairedInterval
                                                )
import           Test.QuickCheck                ( Arbitrary(arbitrary) )
import           Type.Reflection                ( Typeable )
import           Witch                          ( From(..)
                                                , into
                                                , via
                                                )

import           Data.Bifunctor

{- |
The 'Event' type puts a certain amount of structure on
temporally organized data, 
while being flexible in the details.
An 'Event c m a' contains information about
when something occurred (the 'Interval a')
and what occurred (the 'Context m c').
The type parameters @m@, @c@, and @a@ allow to specify 
the types for the 'Context's @m@odel and @c@oncepts
and for the type of the 'Interval' end points.

The 'Event' type parameters are ordered from changing the least often to most often.
A @m@odel tends to be shared across projects.
For example, multiple projects use data from insurance claims, 
and thus share a single model. 
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
>>>
>>> type MyEvent = Event T.Text SomeDomain Integer
>>> let myEvent = event (beginerval 5 0) (context (packConcepts ["foo"]) A Nothing) :: MyEvent
>>> show myEvent
"MkEvent {(0, 5), Context {concepts = Concepts (fromList [Concept \"foo\"]), facts = A, source = Nothing}}"

>>> hasAnyConcepts myEvent (["foo", "duck"] :: [T.Text])
True

>>> hasAllConcepts myEvent (["foo", "duck"] :: [T.Text])
False

>>> data NewDomain = A T.Text | B Integer deriving (Eq, Ord, Show, Generic)
>>> data MyConcepts = Foo | Bar | Baz deriving (Eq, Ord, Show, Generic) 
>>>
>>> type NewEvent = Event MyConcepts NewDomain Integer
>>> let newEvent = event (beginerval 5 0) (context (packConcepts [Foo, Bar]) (A "cool") Nothing) :: NewEvent
>>> show newEvent
"MkEvent {(0, 5), Context {concepts = Concepts (fromList [Concept Foo,Concept Bar]), facts = A \"cool\", source = Nothing}}"

>>> hasConcept newEvent Foo
True

>>> hasConcept newEvent Baz
False

-}

{- tag::eventType[] -}
newtype Event c m a = MkEvent ( PairedInterval (Context c m) a )
{- end::eventType[] -}
  deriving (Eq, Show, Generic)

instance (Ord a) => Intervallic (Event c d) a where
  getInterval (MkEvent x) = getInterval x
  setInterval (MkEvent x) y = MkEvent $ setInterval x y

instance Functor (Event c d) where
  fmap f (MkEvent x) = MkEvent $ fmap f x

instance Bifunctor (Event c) where
  first f (MkEvent x) = MkEvent $ first (fmap f) x
  second f (MkEvent x) = MkEvent $ second f x

instance Ord c => HasConcept (Event c m a) c where
  hasConcept e = hasConcept (getContext e)

instance (Ord a, Ord c, Eq m) => Ord (Event c m a) where
  {-|
  Events are first ordered by their intervals.
  In the case two intervals are equal, 
  the event are ordered by their concepts.
  -}
  compare x y = case ic of
    EQ -> ic
    _  -> compare (getConcepts $ getContext x) (getConcepts $ getContext y)
    where ic = compare (getInterval x) (getInterval y)

instance (NFData a, NFData m, NFData c) => NFData (Event c m a)
instance (Binary m, Binary c, Binary a) => Binary (Event c m a)
-- See NOTE at top of module regarding To/FromJSON instances
instance ( FromJSON a ) => FromJSON (Interval a)
instance ( ToJSON a ) => ToJSON (Interval a)
instance ( FromJSON b, FromJSON a ) => FromJSON (PairedInterval b a)
instance ( ToJSON b, ToJSON a ) => ToJSON (PairedInterval b a)
instance ( Ord c, FromJSON c, FromJSON m, FromJSON a ) => FromJSON (Event c m a)
instance ( Ord c, ToJSON c, ToJSON m, ToJSON a ) => ToJSON (Event c m a)

instance (  Eventable c m a
          , Generic m
          , Typeable c
          , Typeable a
          , Arbitrary m, Arbitrary c, Arbitrary (Interval a)) =>
      Arbitrary (Event c m a) where
  arbitrary = liftM2 event arbitrary arbitrary

instance (Ord a) => From (Event c m a) (Interval a) where
  from = getInterval

-- | A synonym for the basic set of constraints an event needs on its types.
type Eventable c m a = (Eq m, Ord c, Ord a, Show m, Show c, Show a)
  -- Text is not Generic; but c should at least be Typeable

-- | Constraint synonym for @ToJSON@ on an event's component types.
type ToJSONEvent c m a = (ToJSON m, ToJSON c, ToJSON a)

-- | Constraint synonym for @FromSON@ on an event's component types.
type FromJSONEvent c m a = (FromJSON m, FromJSON c, FromJSON a)

-- | A smart constructor for 'Event c m a's.
event :: (Eventable c m a) => Interval a -> Context c m -> Event c m a
event i c = MkEvent (makePairedInterval c i)

-- | Unpack an 'Event' from its constructor.
getEvent :: Event c m a -> PairedInterval (Context c m) a
getEvent (MkEvent x) = x

-- | Get the 'Context' of an 'Event'. 
getContext :: Event c m a -> Context c m
getContext = getPairData . getEvent

{-|
Apply three functions to an 'Event':

1. a function transforming the interval
2. a function transforming the facts
3. a function transforming the concepts

See also: 'bimapContext', 'mapConcepts'.
-}
trimapEvent
  :: (Ord c1, Ord c2)
  => (a1 -> a2)
  -> (c1 -> c2)
  -> (d1 -> d2)
  -> Event c1 d1 a1
  -> Event c2 d2 a2
trimapEvent g f h (MkEvent x) = MkEvent $ bimap (bimapContext f h) g x

{- |
A 'Context' contains information about what ocurred during an 'Event's interval.
This information is carried in context's @concepts@ and/or @facts@.
'Concepts' are set of tags that can be used to identify and filter events
using the 'hasConcept' function
or the related 'hasAnyConcepts' and 'hasAllConcepts' functions.
The @facts@ field contains data of type @m@. 
The @m@ stands for @m@odel, 
meaning the scope and shape of facts
relevant to a particular scientific line of work.
For example, some studies using health care claims data may be sufficiently different
in scope, semanitcs, and aims to warrant having a different collection of facts
from, say, electronic medical records data. 
However, one could create a collection of facts that includes both claims and EHR data.
By having a 'Context' parametrized by the shape of a model,
users are free to define the structure of their facts as needed. 

A context also has a @source@ field,
possibly containing a 'Source',
which carries information about the provenance of the data.

-}
{- tag::contextType[] -}
data Context c m = MkContext
  { -- | the 'Concepts' of a @Context@
    getConcepts :: Concepts c -- <1>
    -- | the facts of a @Context@.  
  , getFacts    :: m -- <2>
    -- | the 'Source' of @Context@
  , getSource   :: Maybe Source -- <3>
  }
  {- end::contextType[] -}
  deriving (Eq, Show, Generic)

instance Ord c => HasConcept (Context c m) c where
  hasConcept c = hasConcept (getConcepts c)

instance (NFData m, NFData c) => NFData (Context c m)
instance (Binary m, Binary c) => Binary (Context c m)
-- See NOTE at top of module regarding To/FromJSON
instance ( Ord c, FromJSON c, FromJSON m ) => FromJSON (Context c m)
instance ( Ord c, ToJSON c, ToJSON m ) => ToJSON (Context c m)

instance ( Arbitrary m, Show m, Eq m, Generic m
         , Arbitrary c, Show c, Eq c, Ord c, Typeable c) =>
      Arbitrary (Context c m) where
  arbitrary = liftM3 MkContext arbitrary arbitrary (pure Nothing)

instance Functor (Context c) where
  fmap f (MkContext c m s) = MkContext c (f m) s

-- | Smart constructor for a 'Context',
context
  :: (Generic m, Ord c, Typeable c)
  => Concepts c
  -> m
  -> Maybe Source
  -> Context c m
context = MkContext

{-|
Apply a two functions to a 'Context':

1. a function transforming the concepts
2. a function transforming the facts

This function is simiilar in flavor to 'Data.Bifunctor.bimap'.
But @Context@ is not a 'Data.Bifunctor.Bifunctor'.
The underlying type of @Concepts@ is 'Data.Set.Set',
which is not a 'Functor' 
due to the @Set@ 'Ord' constraints.
-}
bimapContext
  :: (Ord c1, Ord c2)
  => (c1 -> c2)
  -> (d1 -> d2)
  -> Context c1 d1
  -> Context c2 d2
bimapContext g f (MkContext cpts fcts src) =
  MkContext (mapConcepts g cpts) (f fcts) src

{-|
Turn the 'Source' within a 'Context' to 'Nothing'.
-}
dropSource :: Context c m -> Context c m
dropSource (MkContext cpts fcts _) = MkContext cpts fcts Nothing

{-|
A source may be used to record the provenance of an event from some database.
This data is sometimes useful for debugging.
We generally discourage using @Source@ information in defining features.
-}
data Source = MkSource
  { column   :: Maybe T.Text
  , file     :: Maybe T.Text
  , row      :: Maybe Integer
  , table    :: T.Text
  , database :: T.Text
  }
  deriving (Eq, Show, Generic)

instance NFData Source
instance Binary Source
instance FromJSON Source
instance ToJSON Source

-- | A @Concept@ is simply a tag or label for an 'Event'.
newtype Concept c = MkConcept c deriving (Eq, Ord, Show, Generic)

instance Functor Concept where
  fmap f (MkConcept x) = MkConcept (f x)
instance NFData c => NFData (Concept c)
instance Binary c => Binary (Concept c)
instance FromJSON c => FromJSON (Concept c)
instance ToJSON c => ToJSON (Concept c)

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
newtype Concepts c = MkConcepts ( Set ( Concept c ) )
    deriving (Eq, Show, Ord, Generic)

instance NFData c => NFData (Concepts c)
instance Binary c => Binary (Concepts c)
-- See NOTE at top of module regarding To/FromJSON
instance (Ord c, FromJSON c) => FromJSON (Concepts c)
instance ToJSON c => ToJSON (Concepts c)

instance (Arbitrary c, Ord c) => Arbitrary (Concepts c) where
  arbitrary = fmap packConcepts arbitrary

instance (Ord c) => Semigroup ( Concepts c ) where
  MkConcepts x <> MkConcepts y = MkConcepts (x <> y)

instance (Ord c) => Monoid ( Concepts c ) where
  mempty = MkConcepts mempty

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

-- | A utility for adding concepts to a 'Concepts' from a list.
addConcepts :: (Ord c) => [c] -> Concepts c -> Concepts c
addConcepts x cpts = into x <> cpts

{-|
Apply a function to each 'Concept'
within a 'Concepts' set.

NOTE: 
@Concepts@ are not a 'Functor'.
The underlying type of @Concepts@ is 'Data.Set.Set',
which is not a 'Functor' 
due to the @Set@ 'Ord' constraints.
-}
mapConcepts :: (Ord c1, Ord c2) => (c1 -> c2) -> Concepts c1 -> Concepts c2
mapConcepts f (MkConcepts x) = MkConcepts (Data.Set.map (fmap f) x)

{-| 
The 'HasConcept' typeclass provides predicate functions
for determining whether an @a@ contains a concept.

This class is only used in this 'EventDataTheory.Core' module
for the purposes of having a single @hasConcept@ function
that works on 'Concepts', 'Context', or 'Event' data.
-}
class HasConcept a c where
    -- | Test whether a type @a@ contains a @c@.
    hasConcept  :: a -> c -> Bool

instance (Ord c) => HasConcept (Concepts c) c where
  hasConcept (MkConcepts e) concept = member (MkConcept concept) e

instance (Ord c) => HasConcept (PairedInterval (Concepts c) a) c where
  hasConcept x = hasConcept (getPairData x)

-- | Does an @a@ have *any* of a list of 'Concept's?
hasAnyConcepts :: HasConcept a c => a -> [c] -> Bool
hasAnyConcepts x = any (\c -> x `hasConcept` c)

-- | Does an @a@ have *all* of a list of `Concept's?
hasAllConcepts :: HasConcept a c => a -> [c] -> Bool
hasAllConcepts x = all (\c -> x `hasConcept` c)

{-|
A Concept Interval is simply a synonym for an 'Interval' paired with 'Concepts'.
-}
type ConceptsInterval c a = PairedInterval (Concepts c) a

instance From (Event c m a) (ConceptsInterval c a) where
  from = first getConcepts . getEvent
instance (Ord a) => From (ConceptsInterval c a) (Interval a) where
  from = getInterval

-- | Creates a 'ConceptsInterval` from an `Interval` with empty 'Concepts'.
instance (Ord a, Ord c) => From (Interval a) (ConceptsInterval c a) where
  from = makePairedInterval mempty

-- | Contains a subject identifier
data SubjectID =
    SubjectIDText T.Text
  | SubjectIDInteger Integer
  deriving (Eq, Show, Ord, Generic)

instance Binary SubjectID
instance NFData SubjectID
instance FromJSON SubjectID
instance ToJSON SubjectID
instance From SubjectID T.Text where
  from (SubjectIDText    x) = x
  from (SubjectIDInteger x) = T.pack $ show x
instance From SubjectID Data.Aeson.Value where
  from (SubjectIDText    x) = String x
  from (SubjectIDInteger x) = Number (fromInteger x)
instance From Integer SubjectID where
  from = SubjectIDInteger
instance From T.Text SubjectID where
  from = SubjectIDText

{-|
Provides a common interface to lift a 'Predicate' on some component
of an 'Event' to a 'Predicate (Event c m a)'.
For example, if @x@ is a 'Predicate' on some 'Context m c',
@liftToEventPredicate x@ yields a @Predicate (Event c m a)@, 
thus the predicate then also be applied to @Event@s.

This class is only used in this 'EventDataTheory.Core' module
for the purposes of having a single @liftToEventPredicate@ function
that works on 'Concepts', 'Context', or 'Event' data.
-}
class EventPredicate element c m a where
  {-|
  Lifts a 'Predicate' of a component of an 'Event'
  to a 'Predicate' on an 'Event'
  -}
  liftToEventPredicate :: Predicate element -> Predicate (Event c m a)

instance EventPredicate (Context c m) c m a where
  liftToEventPredicate = contramap getContext

instance EventPredicate m c m a where
  liftToEventPredicate = contramap (getFacts . getContext)

instance EventPredicate (Concepts c) c m a where
  liftToEventPredicate = contramap (getConcepts . getContext)

instance EventPredicate (Maybe Source) c m a where
  liftToEventPredicate = contramap (getSource . getContext)

instance (Ord a) => EventPredicate (Interval a) c m a where
  liftToEventPredicate = contramap getInterval

{-|
Provides a common interface to lift a function
operating on some component of an 'Event'
into a function on an 'Event'. 

This class is only used in this 'EventDataTheory.Core' module
for the purposes of having a single @liftToEventFunction@ function
that works on 'Concepts', 'Context', or 'Event' data.
-}
class EventFunction f c c' m m'  a a' where
  {-|
  Lifts a function @@ of a component of an 'Event'
  to a function on an 'Event'
  -}
  liftToEventFunction :: (Ord c, Ord c') => f -> Event c m a -> Event c' m'  a'

instance EventFunction (c -> c') c c' m m a a where
  liftToEventFunction f = trimapEvent id f id

instance EventFunction (m -> m') c c m m' a a where
  liftToEventFunction = trimapEvent id id

instance EventFunction (Context c m -> Context c' m' ) c c' m m'  a a where
  liftToEventFunction f (MkEvent x) = MkEvent $ first f x

instance EventFunction (a -> a') c c m m a a' where
  liftToEventFunction f = trimapEvent f id id

{-|
Provides a common interface to lift a function
operating on some component of an 'Context'
into a function on an 'Context'. 

This class is only used in this 'EventDataTheory.Core' module
for the purposes of having a single @liftToEventFunction@ function
that works on 'Concepts', 'Context', or 'Event' data.
-}
-- NOTE: this kind of constraint solving could probably be done
-- using the Select monad from Control.Monad.Trans.Select
-- but it's not clear that would add anything other than additional deps.
class ContextFunction f c c' m m'  where
  {-|
  Lifts a function @f@ of a component of an 'Context'
  to a function on an 'Context'
  -}
  liftToContextFunction :: (Ord c, Ord c') => f -> Context c m -> Context c' m' 

instance ContextFunction (Concepts c -> Concepts c') c c' m m where
  liftToContextFunction f (MkContext x y z) = MkContext (f x) y z

instance ContextFunction (c -> c') c c' m m where
  liftToContextFunction f = bimapContext f id

instance ContextFunction (m -> m') c c m m'  where
  liftToContextFunction = bimapContext id
