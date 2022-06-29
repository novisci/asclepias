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

instance (Ord t, Show t, FromDhall t) => TryFrom TestMap (TagSet t) where
  tryFrom input = tagSet
   where
    -- TODO: This awful hack is because lists are not supported by dhallFromCsv.
    -- It means we only support TagSet with one element. You could add
    -- support for multi-element sets by adding a grouping variable to the
    -- schema and collecting "tagSet" values on that variable. Since that
    -- adds some cognitive complexity to the csv input and since most cases
    -- involve just one tag, that method is not used here. You could also
    -- consider rewriting `dhallFromCsv`, largely copying and pasting from the
    -- dhall-csv source. However, the function uses multiple internal types,
    -- which left me stumped as to what to do.
    tagSet = case singleValToList <$> lookup "tagSet" input of
      Nothing        -> Left $ TryFromException input Nothing
      Just (Left  _) -> Left $ TryFromException input Nothing
      Just (Right v) -> first (const err) $ tryFrom @TestVal @(TagSet t) v
    singleValToList x = List . (: []) <$> tryFrom @TestVal x
    err = TryFromException input Nothing

instance (Ord t, FromDhall t, Show t, FromDhall m) => TryFrom TestMap (Context t m) where
  tryFrom input = liftA3 context
                         (first (const err) tagSet)
                         (joinMaybeEither err facts)
                         -- NOTE: ignoring Source in all cases
                         (pure Nothing)
   where
    tagSet = tryFrom input
    facts  = tryFrom @TestVal <$> lookup "facts" input
    err    = TryFromException input Nothing

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

instance (Show a, Ord a, Show t, Ord t, Eq m, Show m, Atomizable a, TryFrom TestMap (Interval a), TryFrom TestMap (Context t m)) => TryFrom TestMap (Event t m a) where
  tryFrom input = liftA2 event (first (const err) i) (first (const err) t)
   where
    i   = tryFrom @TestMap input
    t   = tryFrom @TestMap input
    err = TryFromException input Nothing
