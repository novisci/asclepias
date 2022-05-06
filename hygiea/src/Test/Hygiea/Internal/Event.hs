-- TODO placeholders for event-data-theory general event type and related,
-- specialized to the needs here. these would be imported from edm theory, and this module then would provide the basic conversions for allowed types, so that users who alias the edm theory types don't have to.
--
-- Convenience types to make common conversions simpler
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Test.Hygiea.Internal.Event where

import           Control.Applicative
import           Data.Aeson                     ( ToJSON(..) )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           IntervalAlgebra                ( Interval(..)
                                                , PairedInterval(..)
                                                , makePairedInterval
                                                , parseInterval
                                                )
import           Prelude                 hiding ( lookup )
import           Test.Hygiea.Internal.Atomic
import           Test.Hygiea.Map
import           Witch.From
import           Witch.TryFrom
import           Witch.TryFromException
import           Witch.Utility                  ( tryVia )

newtype Event d c a = MkEvent ( PairedInterval (Context d c) a )
  deriving (Eq, Show, Generic)

data Context d c = MkContext
  -- note the switched order
  { getConcepts :: d
  , getFacts    :: c
  }
  deriving (Eq, Show, Generic)

-- TODO placeholder for a 'common' outcome
data CensoredOccurrence d c a = MkCensoredOccurrence
  { reason :: d
  , time   :: PairedInterval c a
  }
  deriving (Eq, Show, Generic)

-- TODO these are just filling in for IntervalAlgebra, EDM
instance (ToJSON i) => ToJSON (Interval i)
instance (ToJSON d, ToJSON i) => ToJSON (PairedInterval d i)
instance (ToJSON d, ToJSON c) => ToJSON (Context d c)
instance (ToJSON d, ToJSON c, ToJSON a) => ToJSON (Event d c a)
instance (ToJSON d, ToJSON c, ToJSON a) => ToJSON (CensoredOccurrence d c a)

  {- Pre-built conversions.

     So long as inner types are TryFrom TestAtomic, this flattens Event to TestMap.
     In most cases, all a project has to do is to convert their types to Event.

     -}
instance (Atomizable d, Atomizable c) => TryFrom TestMap (Context d c) where
  tryFrom input = liftA2 MkContext
                         (joinMaybeEither err concepts)
                         (joinMaybeEither err facts)
   where
    concepts = tryFrom @TestVal @d <$> lookup "concepts" input
    facts    = tryFrom @TestVal @c <$> lookup "facts" input
    err      = TryFromException input Nothing

instance (Show a, Ord a, Atomizable a, TryFrom TestMap c) => TryFrom TestMap (PairedInterval c a) where
  tryFrom input = liftA2 makePairedInterval
                         (mapLeft err context)
                         (joinEitherOuter err interval)
   where
    context = tryFrom input
    b       = tryFrom @TestVal @a <$> lookup "begin" input
    e       = tryFrom @TestVal @a <$> lookup "end" input
    interval =
      liftA2 parseInterval (joinMaybeEither err b) (joinMaybeEither err e)
    err = TryFromException input Nothing

-- TODO use the PairedInterval instance
instance (Show a, Ord a, Atomizable d, Atomizable c, Atomizable a) => TryFrom TestMap (Event d c a) where
  tryFrom input = MkEvent <$> liftA2 makePairedInterval
                                     (mapLeft err context)
                                     (joinEitherOuter err interval)
   where
    context = tryFrom input
    b       = tryFrom @TestVal @a <$> lookup "begin" input
    e       = tryFrom @TestVal @a <$> lookup "end" input
    interval =
      liftA2 parseInterval (joinMaybeEither err b) (joinMaybeEither err e)
    err = TryFromException input Nothing


instance (Show a, Ord a, Atomizable d, Atomizable a, TryFrom TestMap c) => TryFrom TestMap (CensoredOccurrence d c a) where
  tryFrom input = liftA2 MkCensoredOccurrence
                         (joinMaybeEither err reason)
                         (mapLeft err time)
   where
    reason = tryFrom @TestVal <$> lookup "reason" input
    time   = tryFrom input
    err    = TryFromException input Nothing

  {- TODO these should replace the TryFrom instances above -}

--instance TryFrom TestMap' (Event TestVal TestVal TestVal) where
--  tryFrom input = MkEvent <$> liftA2 makePairedInterval
--                                     (mapLeft err context)
--                                     (joinEitherOuter err interval)
--   where
--    context = tryFrom input
--    b       = lookup "begin" input
--    e       = lookup "end" input
--    interval =
--      liftA2 parseInterval (maybeRight err b) (maybeRight err e)
--    err = TryFromException input Nothing
--
--instance TryFrom TestMap' (Context TestVal TestVal) where
--  tryFrom input = liftA2 MkContext
--                         (maybeRight err concepts)
--                         (maybeRight err facts)
--   where
--    concepts = lookup "concepts" input
--    facts    = lookup "facts" input
--    err      = TryFromException input Nothing


{- UTILITIES -}
-- TODO surely some of these already exist
maybeRight :: e -> Maybe a -> Either e a
maybeRight _   (Just x) = Right x
maybeRight err Nothing  = Left err

-- see Bifunctor
mapLeft :: e -> Either a b -> Either e b
mapLeft err (Left  err') = Left err
mapLeft _   (Right x   ) = Right x

joinMaybeEither :: e -> Maybe (Either a b) -> Either e b
joinMaybeEither err (Just x) = mapLeft err x
joinMaybeEither err Nothing  = Left err

-- collapse nested Either, keeping outer error type but with a possibly new
-- error
joinEitherOuter :: e -> Either e (Either a b) -> Either e b
joinEitherOuter err (Right x) = mapLeft err x
joinEitherOuter _   (Left  x) = Left x

 {- Messin around -}

  {-

-- Convert to context if input map has atomizable concepts and facts fields
instance TryFrom Input (Context InputVal InputVal) where
  tryFrom input = liftA2 MkContext
                         (maybeRight err concepts)
                         (maybeRight err facts)
   where
    concepts = lookup "concepts" input
    facts    = lookup "facts" input
    err      = TryFromException input Nothing

instance (Show d, Show c, Atomizable d, Atomizable c) => From (Context d c) (Context InputVal InputVal) where
  from (MkContext x y) = MkContext (MkInputVal x) (MkInputVal y)

instance (Show d, Show c, Atomizable d, Atomizable c) => TryFrom (Context d c) (Context InputVal InputVal) where
  tryFrom = Right . from

-- would like for this approach to work but some type ambiguity issues I can't figure out
--instance (Show d, Show c, Atomizable d, Atomizable c) => From  (Context InputVal InputVal) (Context d c) where
--
--instance (Show d, Show c, Atomizable d, Atomizable c) => TryFrom (Context InputVal InputVal) (Context d c) where
--  tryFrom = Right . from

--instance (Show d, Show c, Atomizable d, Atomizable c) => TryFrom TestMap (Context d c) where
--  tryFrom = tryVia @(Context InputVal InputVal)


-- doesn't compile because Ord is not well-defiend for InputVal
--instance TryFrom Input (Event InputVal InputVal InputVal) where
--  -- liftA2 not doing what i want here
--  tryFrom input = MkEvent <$> liftA2 makePairedInterval (convert context) (maybeRight err interval)
--    where context = tryFrom input
--          interval = do 
--            b <- lookup "begin" input
--            e <- lookup "end" input
--            case parseInterval b e of
--              Right ii -> Just ii
--              Left _ -> Nothing
--          convert (Right x) = Right x
--          convert (Left _) = Left err
--          err = TryFromException input Nothing

-}