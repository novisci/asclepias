{-| 
Module      : Test.Monarch.Internal.EventDataTheory
Description : Pre-made conversions for @EventDataTheory.Core@ types, to
simplify testing implementation for routines using these types. You should
prefer to use the pre-built conversions needed for Atomizable defined here.
Copyright   : (c) NoviSci, Inc 2022
License     : BSD3
Maintainer  : bbrown@targetrwe.com
  -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Monarch.Internal.EventDataTheory where

import           Control.Applicative
import           Data.Bifunctor                 ( first )
import           Dhall                          ( FromDhall )
import           EventDataTheory
import           Prelude                 hiding ( lookup )
import           Test.Monarch.Internal.Atomic
import           Test.Monarch.Internal.Map
import           Test.Monarch.Internal.Utilities
import           Witch.TryFrom
import           Witch.TryFromException

  {- Pre-built conversions.

     So long as inner types are TryFrom TestAtomic, this flattens Event to TestMap.
     In most cases, all a project has to do is to convert their types to Event.

     -}

instance (Ord c, Show c, FromDhall c) => TryFrom TestMap (Concepts c) where
  tryFrom input = concepts
   where
    -- TODO: This awful hack is because lists are not supported by dhallFromCsv.
    -- It means we only support Concepts with one element. You could add
    -- support for multi-element sets by adding a grouping variable to the
    -- schema and collecting "concepts" values on that variable. Since that
    -- adds some cognitive complexity to the csv input and since most cases
    -- involve just one concept, that method is not used here. You could also
    -- consider rewriting `dhallFromCsv`, largely copying and pasting from the
    -- dhall-csv source. However, the function uses multiple internal types,
    -- which left me stumped as to what to do.
    concepts = case singleValToList <$> lookup "concepts" input of
      Nothing        -> Left $ TryFromException input Nothing
      Just (Left  _) -> Left $ TryFromException input Nothing
      Just (Right v) -> first (const err) $ tryFrom @TestVal @(Concepts c) v
    singleValToList x = List . (: []) <$> tryFrom @TestVal x
    err = TryFromException input Nothing

instance (Ord c, FromDhall c, Show c, FromDhall m) => TryFrom TestMap (Context c m) where
  tryFrom input = liftA3 context
                         (first (const err) concepts)
                         (joinMaybeEither err facts)
                         -- NOTE: ignoring Source in all cases
                         (pure Nothing)
   where
    concepts = tryFrom input
    facts    = tryFrom @TestVal <$> lookup "facts" input
    err      = TryFromException input Nothing

instance (Show a, Ord a, FromDhall a) => TryFrom TestMap (Interval a) where
  tryFrom input = joinEitherOuter err $ liftA2 parseInterval b e
   where
    b   = joinMaybeEither err $ tryFrom @TestVal @a <$> lookup "begin" input
    e   = joinMaybeEither err $ tryFrom @TestVal @a <$> lookup "end" input
    err = TryFromException input Nothing

instance (Show a, Ord a, FromDhall a, TryFrom TestMap c) => TryFrom TestMap (PairedInterval c a) where
  tryFrom input = liftA2 makePairedInterval
                         (first (const err) ctx)
                         (first (const err) interval)
   where
     -- Note this need not actually be Context here.
    ctx      = tryFrom input
    interval = tryFrom input
    err      = TryFromException input Nothing

instance (Show a, Ord a, Show c, Ord c, Eq m, Show m, Atomizable a, TryFrom TestMap (Interval a), TryFrom TestMap (Context c m)) => TryFrom TestMap (Event c m a) where
  tryFrom input = liftA2 event (first (const err) i) (first (const err) c)
   where
    i   = tryFrom @TestMap input
    c   = tryFrom @TestMap input
    err = TryFromException input Nothing