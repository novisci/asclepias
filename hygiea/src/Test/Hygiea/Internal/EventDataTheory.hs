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
  {-

data Context d c = MkContext
  -- note the switched order
  { getConcepts :: d
  , getFacts    :: c
  }
  deriving (Eq, Show, Generic)

-}

  {- TODO comments on Concepts parsing 
      * This is awful.
      * Dhall schema for csv input should declare 'concepts' field as Text
      * The internal concept type must have a FromDhall instance, and the Text
      will be converted via dhall's interpreter
      -}


  {- Pre-built conversions.

     So long as inner types are TryFrom TestAtomic, this flattens Event to TestMap.
     In most cases, all a project has to do is to convert their types to Event.

     -}
-- TODO If this list-like interface is kept, the constraint probably should be
-- Binary

-- | This tries to convert a @Text@ of format "x,y,z" with possible whitespace
-- before/after the commas into a @c@ via @Read@, with failure type given by
-- the caller. Solely to be used in @TryFrom TestVal (Concepts c)@. The
-- functionality is far from ideal and is purely a means to handle the fact
-- that a set of concepts will be entered in flattened form in the text input
-- csv that gets converted to @TestVal@. Empty string input returns @Left@,
-- since presumably you cannot @Read@ something from nothing.
tryReadTextList :: (Read c) => e -> Text -> Either e [c]
tryReadTextList err ""  = Left err
tryReadTextList err txt = traverse (tryR err) $ splitOn "," txt
 where
  tryR err' "" = Left err
  tryR err' t  = Right $ read $ unpack t

instance (Read c, Ord c) => TryFrom TestVal (Concepts c) where
  tryFrom (Atomic (TText v)) = packConcepts <$> tryReadTextList err v
    where err = TryFromException (Atomic (TText v)) Nothing
  tryFrom x = Left $ TryFromException x Nothing

instance (Read c, Ord c, Atomizable c, Atomizable m) => TryFrom TestMap (Context c m) where
  tryFrom input = liftA3 context
                         (joinMaybeEither err concepts)
                         (joinMaybeEither err facts)
                         -- NOTE: ignoring Source in all cases
                         (pure Nothing)
   where
    concepts = tryFrom @TestVal <$> lookup "concepts" input
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
