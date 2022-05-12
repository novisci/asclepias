-- | Pre-made conversions for EventDataTheory types, to simplify testing implementation for routines using these types. You should prefer to use the pre-built conversions needed for Atomizable defined here.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Test.Monarch.Internal.EventDataTheory where

import           Control.Applicative
import           Data.Bifunctor                 ( first )
import           Data.Text                      ( Text
                                                , splitOn
                                                , strip
                                                , unpack
                                                )
import           Dhall                          ( FromDhall
                                                , ToDhall
                                                )
import           EventDataTheory.Core
import           IntervalAlgebra                ( Interval(..)
                                                , PairedInterval(..)
                                                , makePairedInterval
                                                , parseInterval
                                                )
import           Prelude                 hiding ( lookup )
import           Test.Monarch.MonarchException
import           Test.Monarch.Internal.Atomic
import           Test.Monarch.Internal.Map
import           Test.Monarch.Internal.Utilities
import           Witch.From
import           Witch.TryFrom
import           Witch.TryFromException
import           Witch.Utility                  ( tryVia )

  {- Pre-built conversions.

     So long as inner types are TryFrom TestAtomic, this flattens Event to TestMap.
     In most cases, all a project has to do is to convert their types to Event.

     -}

--instance (Ord c, FromDhall c) => TryFrom TestVal (Concepts c) where
--  tryFrom input = packConcepts <$> traverse (first (const err)) [tryFrom @TestVal input]
--    where err = TryFromException input Nothing

--instance (Ord c, TryFrom c TestAtomic) => TryFrom (Concepts c) TestVal where
--  tryFrom input = first (const err) $ List <$> traverse
--    tryFrom
--    (from @(Concepts c) @[c] input)
--    where err = TryFromException input Nothing

-- TODO revisit these. this is really the way to do it and avoids some overlapping instances.
instance (ToDhall c) => ToDhall (Concept c)
instance (FromDhall c) => FromDhall (Concept c)
instance (ToDhall c) => ToDhall (Concepts c)
instance (FromDhall c, Ord c, Show c) => FromDhall (Concepts c)

instance (Ord c, TryFrom TestVal (Concepts c)) => TryFrom TestMap (Concepts c) where
  tryFrom input = concepts
   where
    -- TODO: This awful hack is because lists are not supported by dhallFromCsv.
    -- It means we only support Concepts with one element.
    concepts = case singleValToList <$> lookup "concepts" input of
                 Nothing -> Left $ TryFromException input Nothing
                 Just (Left _) -> Left $ TryFromException input Nothing
                 Just (Right v) -> first (const err) $ tryFrom @TestVal @(Concepts c) v 
    singleValToList x = List . (: []) <$> tryFrom @TestVal x
    err      = TryFromException input Nothing

instance (Ord c, Atomizable (Concepts c), Atomizable m) => TryFrom TestMap (Context c m) where
  tryFrom input = liftA3 context
                         (first (const err) concepts)
                         (joinMaybeEither err facts)
                         -- NOTE: ignoring Source in all cases
                         (pure Nothing)
   where
    concepts = tryFrom input
    facts    = tryFrom @TestVal <$> lookup "facts" input
    err      = TryFromException input Nothing

instance (Show a, Ord a, Atomizable a) => TryFrom TestMap (Interval a) where
  tryFrom input = joinEitherOuter err $ liftA2 parseInterval b e
   where
    b   = joinMaybeEither err $ tryFrom @TestVal @a <$> lookup "begin" input
    e   = joinMaybeEither err $ tryFrom @TestVal @a <$> lookup "end" input
    err = TryFromException input Nothing

instance (Show a, Ord a, Atomizable a, TryFrom TestMap c) => TryFrom TestMap (PairedInterval c a) where
  tryFrom input = liftA2 makePairedInterval
                         (first (const err) context)
                         (first (const err) interval)
   where
     -- Note this need not actually be Context here.
    context  = tryFrom input
    interval = tryFrom input
    err      = TryFromException input Nothing

instance (Show a, Ord a, Show c, Ord c, Eq m, Show m, Atomizable a, TryFrom TestMap (Interval a), TryFrom TestMap (Context c m)) => TryFrom TestMap (Event c m a) where
  tryFrom input = liftA2 event (first (const err) i) (first (const err) c)
   where
    i   = tryFrom @TestMap input
    c   = tryFrom @TestMap input
    err = TryFromException input Nothing

  {- An outcome type from Hasklepias Misc -}
    {-
-- TODO placeholder for a 'common' outcome
data CensoredOccurrence d c a = MkCensoredOccurrence
  { reason :: d
  , time   :: PairedInterval c a
  }
  deriving (Eq, Show, Generic)
instance (Show a, Ord a, Atomizable d, Atomizable a, TryFrom TestMap c) => TryFrom TestMap (CensoredOccurrence d c a) where
  tryFrom input = liftA2 MkCensoredOccurrence
                         (joinMaybeEither err reason)
                         (mapLeft err time)
   where
    reason = tryFrom @TestVal <$> lookup "reason" input
    time   = tryFrom input
    err    = TryFromException input Nothing
-}
