{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Hasklepias Event Type
-- Description : Defines the Event type and its component types, constructors,
--               and class instance
-- Copyright   : (c) Target RWE 2023
-- License     : BSD3
-- Maintainer  : bbrown@targetrwe.com 
--               ljackman@targetrwe.com 
--               dpritchard@targetrwe.com
--
-- NOTE: The types herein are how events are represently internally.
-- Events may be represented in different structures for transferring or storing data, for example.
-- The To/FromJSON instances for types defined in this module are derived generically.
-- These can be useful for writing tests, for example, but
-- they are not designed to encode/decode data in the new line delimited format
-- defined in the
-- [event data model docs](https://docs.novisci.com/event-data/3.0/index.html)
-- See the neighboring EventLine module for types and To/FromJSON instances
-- designed for the purpose of marshaling data from JSON lines.
module EventDataTheory.Core
  ( Event,
    event,
    getEvent,
    getContext,
    Source (..),
    Tag,
    TagSet,
    Context,
    TagSetInterval,
    getFacts,
    getSource,
    getTagSet,
    context,
    toTagSet,
    packTag,
    unpackTag,
    packTagSet,
    unpackTagSet,
    hasTag,
    hasAnyTag,
    hasAllTags,
    addTagSet,
    liftToEventPredicate,
    liftToEventFunction,
    liftToContextFunction,
    bimapContext,
    mapTagSet,
    dropSource,
    SubjectID,
    -- TODO: evaluate this old note. does not seem that these should
    -- be exported if that is the only reason they are now.
    --
    -- the following names are exported for haddock linking
    HasTag,
    EventPredicate,
    Eventable,
    FromJSONEvent,
    ToJSONEvent,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (liftM2, liftM3)
import Data.Aeson
  ( FromJSON,
    ToJSON,
    Value (Number, String),
  )
import Data.Bifunctor
import Data.Binary (Binary)
import Data.Functor.Contravariant
  ( Contravariant (contramap),
    Predicate (..),
  )
import Data.Set (Set, fromList, map, member, toList)
import qualified Data.Text as T
import Dhall (FromDhall, ToDhall)
import GHC.Generics (Generic)
import IntervalAlgebra
  ( Interval,
    Intervallic (..),
    PairedInterval,
    getPairData,
    makePairedInterval,
  )
import Test.Tasty.QuickCheck (Arbitrary (arbitrary))
import Type.Reflection (Typeable)
import Witch (From (..), into, via)

-- |
-- The 'Event' type puts a certain amount of structure on
-- temporally organized data,
-- while being flexible in the details.
-- An 'Event t m a' contains information about
-- when something occurred (the 'Interval a')
-- and what occurred (the 'Context m t').
-- The type parameters @m@, @t@, and @a@ allow to specify
-- the types for the 'Context's @m@odel and @t@agSet
-- and for the type of the 'Interval' end points.
--
-- The 'Event' type parameters are ordered from changing the least often to most often.
-- A @m@odel tends to be shared across projects.
-- For example, multiple projects use data from insurance claims,
-- and thus share a single model.
-- A project often defines its own @t@agSet,
-- though tag sets can be shared across projects.
-- Within a project, multiple 'Interval' types may used.
-- Data may be imported as 'Interval Day',
-- but then modified to 'Interval Integer' based on some reference point.
--
-- The contents of a 'Context' are explained in a separate section,
-- but we give a couple examples of using events here.
--
-- The 'event' function is a smart constructor for 'Event'.
--
-- >>> :set -XOverloadedStrings
-- >>> import IntervalAlgebra ( beginerval )
--
-- >>> data SomeModel = A | B deriving (Eq, Ord, Show, Generic)
-- >>>
-- >>> type MyEvent = Event T.Text SomeModel Integer
-- >>> let myEvent = event (beginerval 5 0) (context (packTagSet ["foo"]) A Nothing) :: MyEvent
-- >>> show myEvent
-- "MkEvent {(0, 5), MkContext {getTagSet = MkTagSet (fromList [MkTag \"foo\"]), getFacts = A, getSource = Nothing}}"
--
-- >>> hasAnyTag myEvent (["foo", "duck"] :: [T.Text])
-- True
--
-- >>> hasAllTags myEvent (["foo", "duck"] :: [T.Text])
-- False
--
-- >>> data NewModel = A T.Text | B Integer deriving (Eq, Ord, Show, Generic)
-- >>> data MyTagSet = Foo | Bar | Baz deriving (Eq, Ord, Show, Generic)
-- >>>
-- >>> type NewEvent = Event MyTagSet NewModel Integer
-- >>> let newEvent = event (beginerval 5 0) (context (packTagSet [Foo, Bar]) (A "cool") Nothing) :: NewEvent
-- >>> show newEvent
-- "MkEvent {(0, 5), MkContext {getTagSet = MkTagSet (fromList [MkTag Foo,MkTag Bar]), getFacts = A \"cool\", getSource = Nothing}}"
--
-- >>> hasTag newEvent Foo
-- True
--
-- >>> hasTag newEvent Baz
-- False

{- tag::eventType[] -}
newtype Event t m a = MkEvent (PairedInterval (Context t m) a)
  {- end::eventType[] -}
  deriving (Eq, Show, Generic)

instance Intervallic (Event t m) where
  getInterval (MkEvent x) = getInterval x
  setInterval (MkEvent x) y = MkEvent $ setInterval x y

instance Ord t => HasTag (Event t m a) t where
  hasTag e = hasTag (getContext e)

instance (Ord a, Ord t, Eq m) => Ord (Event t m a) where
  -- \|
  --  Events are first ordered by their intervals.
  --  In the case two intervals are equal,
  --  the event are ordered by their tagSet.
  compare x y = case ic of
    EQ -> compare (getTagSet $ getContext x) (getTagSet $ getContext y)
    _ -> ic
    where
      ic = compare (getInterval x) (getInterval y)

instance (NFData a, NFData m, NFData t) => NFData (Event t m a)

instance (Binary m, Binary t, Binary a) => Binary (Event t m a)

instance (FromJSON a) => FromJSON (Interval a)

instance (ToJSON a) => ToJSON (Interval a)

instance (FromJSON b, FromJSON a) => FromJSON (PairedInterval b a)

instance (ToJSON b, ToJSON a) => ToJSON (PairedInterval b a)

instance (Ord t, FromJSON t, FromJSON m, FromJSON a) => FromJSON (Event t m a)

instance (Ord t, ToJSON t, ToJSON m, ToJSON a) => ToJSON (Event t m a)

instance
  ( 
    Ord t,
    Arbitrary m,
    Arbitrary t,
    Arbitrary (Interval a)
  ) =>
  Arbitrary (Event t m a)
  where
  arbitrary = liftM2 event arbitrary arbitrary

instance From (Event t m a) (Interval a) where
  from = getInterval

-- TODO: revisit this constraint synonym. these likely are not needed.
-- this pattern appears to be part of a design approach that constrains
-- constructors such as `event` not because that constructor actually needs
-- the given constraints but because imagined uses of values of the type
-- will require the constraints. however, it almost always is cleaner to
-- place the restrictions where they actually are used.

-- | A synonym for a basic set of constraints frequently used with
-- the 'Event' type.
type Eventable t m a = (Eq m, Ord t, Ord a, Show m, Show t, Show a)

-- | Constraint synonym for @ToJSON@ on an event's component types.
type ToJSONEvent t m a = (ToJSON m, ToJSON t, ToJSON a)

-- | Constraint synonym for @FromSON@ on an event's component types.
type FromJSONEvent t m a = (FromJSON m, FromJSON t, FromJSON a)

-- | A smart constructor for 'Event t m a's.
event :: Interval a -> Context t m -> Event t m a
event i t = MkEvent (makePairedInterval t i)

-- | Unpack an 'Event' from its constructor.
getEvent :: Event t m a -> PairedInterval (Context t m) a
getEvent (MkEvent x) = x

-- | Get the 'Context' of an 'Event'.
getContext :: Event t m a -> Context t m
getContext = getPairData . getEvent

-- |
-- A 'Context' contains information about what ocurred during an 'Event's interval.
-- This information is carried in context's @tagSet@ and/or @facts@.
-- 'TagSet' are set of tags that can be used to identify and filter events
-- using the 'hasTag' function
-- or the related 'hasAnyTag' and 'hasAllTags' functions.
-- The @facts@ field contains data of type @m@.
-- The @m@ stands for @m@odel,
-- meaning the scope and shape of facts
-- relevant to a particular scientific line of work.
-- For example, some studies using health care claims data may be sufficiently different
-- in scope, semanitcs, and aims to warrant having a different collection of facts
-- from, say, electronic medical records data.
-- However, one could create a collection of facts that includes both claims and EHR data.
-- By having a 'Context' parametrized by the shape of a model,
-- users are free to define the structure of their facts as needed.
--
-- A context also has a @source@ field,
-- possibly containing a 'Source',
-- which carries information about the provenance of the data.

{- tag::contextType[] -}
data Context t m = MkContext
  { -- | the 'TagSet' of a @Context@
    getTagSet :: TagSet t, -- <1>

    -- | the facts of a @Context@.
    getFacts :: m, -- <2>

    -- | the 'Source' of @Context@
    getSource :: Maybe Source -- <3>
  }
  {- end::contextType[] -}
  deriving (Eq, Show, Generic)


instance (Ord t) => HasTag (Context t m) t where
  hasTag t = hasTag (getTagSet t)

instance (NFData m, NFData t) => NFData (Context t m)

instance (Binary m, Binary t) => Binary (Context t m)

-- NOTE: Ord t is required because of TagSet.
instance (Ord t, FromJSON t, FromJSON m) => FromJSON (Context t m)

instance (Ord t, ToJSON t, ToJSON m) => ToJSON (Context t m)

-- | The 'Arbitrary' instance for 'Context' fixes 'getSource' to 'Nothing'.
instance
  ( Arbitrary m,
    Arbitrary t,
    Ord t
  ) =>
  Arbitrary (Context t m)
  where
  arbitrary = liftM3 MkContext arbitrary arbitrary (pure Nothing)

-- | The 'Functor' instance of @Context t@ maps over the 'getFacts'
-- field, leaving 'getSource' untouched.
instance Functor (Context t) where
  fmap f (MkContext t m s) = MkContext t (f m) s

-- | Smart constructor for a 'Context',
context :: TagSet t -> d -> Maybe Source -> Context t d
context = MkContext

-- |
-- Apply a two functions to a 'Context':
--
-- 1. a function transforming the tagSet
-- 2. a function transforming the facts
--
-- This function is simiilar in flavor to 'Data.Bifunctor.bimap'.
-- But @Context@ is not a 'Data.Bifunctor.Bifunctor'.
-- The underlying type of @TagSet@ is 'Data.Set.Set',
-- which is not a 'Functor'
-- because of the @Set@ 'Ord' constraints.
bimapContext ::
  (Ord t2) =>
  (t1 -> t2) ->
  (d1 -> d2) ->
  Context t1 d1 ->
  Context t2 d2
bimapContext g f (MkContext tSet fcts src) =
  MkContext (mapTagSet g tSet) (f fcts) src

-- |
-- Turn the 'Source' within a 'Context' to 'Nothing'.
dropSource :: Context t m -> Context t m
dropSource (MkContext tSet fcts _) = MkContext tSet fcts Nothing

-- |
-- A @Source@ may be used to record the source of an event from a database.
-- This data is sometimes useful for debugging.
-- We generally discourage using @Source@ information in defining features.
data Source = MkSource
  { column :: Maybe T.Text,
    file :: Maybe T.Text,
    row :: Maybe Integer,
    table :: T.Text,
    database :: T.Text
  }
  deriving (Eq, Show, Generic)

instance NFData Source

instance Binary Source

instance FromJSON Source

instance ToJSON Source

-- | A @Tag@ is simply a label for an 'Event'.
newtype Tag t = MkTag t deriving (Eq, Ord, Show, Generic)

instance Functor Tag where
  fmap f (MkTag x) = MkTag (f x)

instance NFData t => NFData (Tag t)

instance Binary t => Binary (Tag t)

instance FromJSON t => FromJSON (Tag t)

instance ToJSON t => ToJSON (Tag t)

instance (ToDhall t) => ToDhall (Tag t)

instance (FromDhall t) => FromDhall (Tag t)

instance From (Tag t) t

instance From t (Tag t)

-- | Wrap value as a Tag
packTag :: t -> Tag t
packTag = into

-- | Unwrap a value from a Tag
unpackTag :: Tag t -> t
unpackTag = into

-- |
-- @TagSet t@ is a 'Set' of 'Tag t's.
-- TagSet inherit the monoidal properties of 'Set', by 'Data.Set.union'.
newtype TagSet t = MkTagSet (Set (Tag t))
  deriving (Eq, Show, Ord, Generic)

instance NFData t => NFData (TagSet t)

instance Binary t => Binary (TagSet t)

-- See NOTE at top of module regarding To/FromJSON
instance (Ord t, FromJSON t) => FromJSON (TagSet t)

instance ToJSON t => ToJSON (TagSet t)

instance (ToDhall t) => ToDhall (TagSet t)

instance (FromDhall t, Ord t, Show t) => FromDhall (TagSet t)

instance (Arbitrary t, Ord t) => Arbitrary (TagSet t) where
  arbitrary = fmap packTagSet arbitrary

instance (Ord t) => Semigroup (TagSet t) where
  MkTagSet x <> MkTagSet y = MkTagSet (x <> y)

instance (Ord t) => Monoid (TagSet t) where
  mempty = MkTagSet mempty

instance (Ord t) => From (TagSet t) (Set (Tag t))

instance (Ord t) => From (Set (Tag t)) (TagSet t)

instance (Ord t) => From (Set (Tag t)) [t] where
  from x = from @(Set t) (Data.Set.map (from @(Tag t)) x)

instance (Ord t) => From [t] (Set (Tag t)) where
  from x = from @[Tag t] (fmap from x)

instance (Ord t) => From (TagSet t) [t] where
  from = via @(Set (Tag t))

instance (Ord t) => From [t] (TagSet t) where
  from = via @(Set (Tag t))

-- | Put a list of values into a set of tagSet.
packTagSet :: Ord t => [t] -> TagSet t
packTagSet = from

-- | Take a tag set to a list of values.
unpackTagSet :: (Ord t) => TagSet t -> [t]
unpackTagSet = from

-- | Constructor for 'TagSet'.
toTagSet :: (Ord t) => Set (Tag t) -> TagSet t
toTagSet = from

-- | A utility for adding tag sets to a 'TagSet' from a list.
addTagSet :: (Ord t) => [t] -> TagSet t -> TagSet t
addTagSet x tSet = into x <> tSet

-- |
-- Apply a function to each 'Tag'
-- within a 'TagSet' set.
--
-- NOTE:
-- @TagSet@ are not a 'Functor'.
-- The underlying type of @TagSet@ is 'Data.Set.Set',
-- which is not a 'Functor'
-- due to the @Set@ 'Ord' constraints.
mapTagSet :: (Ord t2) => (t1 -> t2) -> TagSet t1 -> TagSet t2
mapTagSet f (MkTagSet x) = MkTagSet (Data.Set.map (fmap f) x)

-- |
-- The 'HasTag' typeclass provides predicate functions
-- for determining whether an @a@ contains a tag.
--
-- This class is only used in this 'EventDataTheory.Core' module
-- for the purposes of having a single @hasTag@ function
-- that works on 'TagSet', 'Context', or 'Event' data.
class HasTag a t where
  -- | Test whether a type @a@ contains a @t@.
  hasTag :: a -> t -> Bool

instance (Ord t) => HasTag (TagSet t) t where
  hasTag (MkTagSet e) tag = member (MkTag tag) e

instance (Ord t) => HasTag (PairedInterval (TagSet t) a) t where
  hasTag x = hasTag (getPairData x)

-- | Does an @a@ have *any* of a list of 'Tag's?
hasAnyTag :: HasTag a t => a -> [t] -> Bool
hasAnyTag x = any (\t -> x `hasTag` t)

-- | Does an @a@ have *all* of a list of `Tag's?
hasAllTags :: HasTag a t => a -> [t] -> Bool
hasAllTags x = all (\t -> x `hasTag` t)

-- |
-- A Tag Interval is simply a synonym for an 'Interval' paired with 'TagSet'.
type TagSetInterval t a = PairedInterval (TagSet t) a

instance From (Event t m a) (TagSetInterval t a) where
  from x = makePairedInterval (getTagSet $ getContext x) (getInterval x)

instance (Ord a) => From (TagSetInterval t a) (Interval a) where
  from = getInterval

-- | Creates a 'TagSetInterval` from an `Interval` with empty 'TagSet'.
instance (Ord t, Ord t) => From (Interval a) (TagSetInterval t a) where
  from = makePairedInterval mempty

-- | Contains a subject identifier
type SubjectID = T.Text

-- |
-- Provides a common interface to lift a @'Predicate' e@ to a
-- @Predicate (Event t m a)@.
--
-- For example, if @x@ is a 'Predicate' on some 'Context m t',
-- @liftToEventPredicate x@ yields a @Predicate (Event t m a)@,
-- thus the predicate then also be applied to @Event@s.
--
-- This class is only used in this 'EventDataTheory.Core' module
-- for the purposes of having a single @liftToEventPredicate@ function
-- that works on 'TagSet', 'Context', or 'Event' data.
class EventPredicate element t m a where
  -- |
  --  Lifts a 'Predicate' of a component of an 'Event'
  --  to a 'Predicate' on an 'Event'
  liftToEventPredicate :: Predicate element -> Predicate (Event t m a)

instance EventPredicate (Context t m) t m a where
  liftToEventPredicate = contramap getContext

instance EventPredicate m t m a where
  liftToEventPredicate = contramap (getFacts . getContext)

instance EventPredicate (TagSet t) t m a where
  liftToEventPredicate = contramap (getTagSet . getContext)

instance EventPredicate (Maybe Source) t m a where
  liftToEventPredicate = contramap (getSource . getContext)

instance EventPredicate (Interval a) t m a where
  liftToEventPredicate = contramap getInterval

-- |
-- Provides a common interface to lift a function
-- operating on some component of an 'Event'
-- into a function on an 'Event'.
--
-- This class is only used in this 'EventDataTheory.Core' module
-- for the purposes of having a single @liftToEventFunction@ function
-- that works on 'TagSet', 'Context', or 'Event' data.
class EventFunction f t t' m m' a a' where
  -- |
  --  Lifts a function @@ of a component of an 'Event'
  --  to a function on an 'Event'
  liftToEventFunction :: (Ord t, Ord t') => f -> Event t m a -> Event t' m' a'

instance EventFunction (t -> t') t t' m m a a where
  liftToEventFunction f x =
    MkEvent $
      makePairedInterval (bimapContext f id $ getContext x) (getInterval x)

instance EventFunction (m -> m') t t m m' a a where
  liftToEventFunction f x =
    MkEvent $
      makePairedInterval (bimapContext id f $ getContext x) (getInterval x)

instance EventFunction (Context t m -> Context t' m') t t' m m' a a where
  liftToEventFunction f x =
    MkEvent $ makePairedInterval (f $ getContext x) (getInterval x)

-- |
-- Provides a common interface to lift a function
-- operating on some component of an 'Context'
-- into a function on an 'Context'.
--
-- This class is only used in this 'EventDataTheory.Core' module
-- for the purposes of having a single @liftToEventFunction@ function
-- that works on 'TagSet', 'Context', or 'Event' data.

-- NOTE: this kind of constraint solving could probably be done
-- using the Select monad from Control.Monad.Trans.Select
-- but it's not clear that would add anything other than additional deps.
class ContextFunction f t t' m m' where
  -- |
  --  Lifts a function @f@ of a component of an 'Context'
  --  to a function on an 'Context'
  liftToContextFunction :: (Ord t, Ord t') => f -> Context t m -> Context t' m'

instance ContextFunction (TagSet t -> TagSet t') t t' m m where
  liftToContextFunction f (MkContext x y z) = MkContext (f x) y z

instance ContextFunction (t -> t') t t' m m where
  liftToContextFunction f = bimapContext f id

instance ContextFunction (m -> m') t t m m' where
  liftToContextFunction = bimapContext id
