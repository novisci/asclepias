{-|
Module      : Stype continuous types
Description : Statistical types
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}

{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric #-}

module Stype.Numeric.Continuous (
    Continuous(..)
  , NonnegContinuous(..)
  , EventTime(..)
  , mkEventTime
) where

import safe Control.Applicative           ( Applicative(liftA2) )
import safe GHC.Generics                  ( Generic )

-- | Data type for continuous numbers.
data Continuous a = NegContInf | Cont a | ContInf
  deriving (Eq, Show, Ord, Generic)

instance Functor Continuous where
  fmap f (Cont x) = Cont (f x)
  fmap f NegContInf = NegContInf
  fmap f ContInf = ContInf

instance Applicative Continuous where
  pure x = Cont x
  (<*>) (Cont f) (Cont x) =  Cont (f x)
  (<*>) NegContInf (Cont x) =  NegContInf
  (<*>) ContInf (Cont x) =  ContInf
  (<*>) _ NegContInf =  NegContInf
  (<*>) _ ContInf =  ContInf

instance Num a => Num (Continuous a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*) 
  abs = fmap abs
  fromInteger x = Cont $ fromInteger x
  signum = fmap signum
  negate = fmap negate

-- | Data type for nonnegative continuous numbers.
data NonnegContinuous a = NonNegCont a | NonNegContInf
  deriving (Eq, Show, Ord, Generic)

data ParseErrorNegative = ParseErrorNegative deriving (Eq, Show)

-- | Parse a number into a Nonnegative continuous number.
parseNonnegCont :: (Num a, Ord a) => a -> Either ParseErrorNegative (NonnegContinuous a)
parseNonnegCont x 
  | x < 0 = Left ParseErrorNegative
  | otherwise = Right (NonNegCont x)

-- | Data type for event times.
newtype EventTime a = EventTime { getEventTime :: NonnegContinuous a }
  deriving (Eq, Show, Ord, Generic)

-- | Create an event time from a @Maybe@.
mkEventTime :: Maybe a -> EventTime a
mkEventTime (Just x) = EventTime $ NonNegCont x
mkEventTime Nothing  = EventTime NonNegContInf
