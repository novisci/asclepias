{-|
Module      : Stype continuous types
Description : Statistical types
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}

{-# LANGUAGE Safe #-}
module Stype.Numeric.Continuous (
    Continuous(..)
  , NonnegContinuous(..)
  , EventTime(..)
  , mkEventTime
) where

import safe Control.Applicative           ( Applicative(liftA2) )


data Continuous a = NegContInf | Cont a | ContInf
  deriving (Eq, Show, Ord)

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

data NonnegContinuous a = NonNeg a | NonNegInf
  deriving (Eq, Show, Ord)

newtype EventTime a = EventTime { getEventTime :: NonnegContinuous a }
  deriving (Eq, Show, Ord)

mkEventTime :: Maybe a -> EventTime a
mkEventTime (Just x) = EventTime $ NonNeg x
mkEventTime Nothing  = EventTime NonNegInf