-- | Pre-made conversions for EventDataTheory types, to simplify testing implementation for routines using these types. You should prefer to use the pre-built conversions needed for Atomizable defined here.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Test.Hygiea.Internal.EventDataTheory where

import           Control.Applicative
import           Data.Bifunctor                 ( first )
import           Data.Text                      ( Text
                                                , splitOn
                                                , strip
                                                , unpack
                                                )
import           Dhall                          ( FromDhall )
import           EventDataTheory.Core
import           IntervalAlgebra                ( Interval(..)
                                                , PairedInterval(..)
                                                , makePairedInterval
                                                , parseInterval
                                                )
import           Prelude                 hiding ( lookup )
import           Test.Hygiea.HygieaException
import           Test.Hygiea.Internal.Atomic
import           Test.Hygiea.Internal.Map
import           Test.Hygiea.Internal.Utilities
import           Witch.From
import           Witch.TryFrom
import           Witch.TryFromException
import           Witch.Utility                  ( tryVia )

  {- Pre-built conversions.

     So long as inner types are TryFrom TestAtomic, this flattens Event to TestMap.
     In most cases, all a project has to do is to convert their types to Event.

     -}

-- TODO check whether this shows up as orphan
instance (Ord c, TryFrom TestAtomic c) => TryFrom TestVal (Concepts c) where
  tryFrom (List xs) = fmap packConcepts $ first (const err) $ traverse
    (tryFrom @TestAtomic @c)
    xs
    where err = TryFromException (List xs) Nothing
  tryFrom input = Left $ TryFromException input Nothing

instance (Ord c, Atomizable (Concepts c), Atomizable m) => TryFrom TestMap (Context c m) where
  tryFrom input = liftA3 context
                         (joinMaybeEither err concepts)
                         (joinMaybeEither err facts)
                         -- NOTE: ignoring Source in all cases
                         (pure Nothing)
   where
    concepts = tryFrom @TestVal @(Concepts c) <$> lookup "concepts" input
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
