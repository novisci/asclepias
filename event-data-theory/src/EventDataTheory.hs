{-|
Module      : Hasklepias Event Type
Description : Defines the Event type and its component types, constructors, 
              and class instance
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module EventDataTheory
  (
  ) where

import IntervalAlgebra
import qualified Data.Text as T
import GHC.Generics ( Generic )


data Context d c = Context
  { concepts :: c
  , facts    :: d
  , source   :: Maybe Source
  }
  deriving (Eq, Show, Generic)

data Source = Source 
    { column :: Maybe T.Text
    , file   :: Maybe T.Text
    , row    :: Maybe Integer
    , table  :: T.Text
    , database :: T.Text
    }
  deriving (Eq, Show, Generic)

newtype Event d c a = MkEvent { getEvent ::  PairedInterval (Context d c) a }
  deriving (Eq, Show, Generic)

instance (Ord a) => Intervallic (Event d c) a where
  getInterval (MkEvent x) =  getInterval x
  setInterval (MkEvent x) y = MkEvent $ setInterval x y

bar :: (Eq a, Eq c, Eq d) => Event d c a -> [Event d c a] -> [Event d c a]
bar x = filter (x ==)

makePairPredicate
  :: (Ord a)
  => ComparativePredicateOf2 (i0 a) ((Event d c) a)
  -> i0 a
  -> (Context d c -> Bool)
  -> (Event d c a -> Bool)
makePairPredicate pi i pd x = pi i x && pd (getPairData (getEvent x))

data ClaimsDomain =
    Enrollment
  | Medication
  deriving (Eq, Show, Generic) 

type ClaimsEvent c a = Event ClaimsDomain c a

data PrjConcepts =
    Diabetes
  | HeartFailure

type PrjEvent a = ClaimsEvent PrjConcepts a

{--}

class ( Intervallic (e (Context c d)) a
      , Eq (e (Context c d) a)
      , Generic (e (Context c d) a)
      ) => 
  EventModel2 e c d a where

newtype MyEvent2 b a = MkMyEvent2 (PairedInterval b a) deriving (Eq, Generic)

instance (Ord a) => Intervallic (MyEvent2 b) a where
  getInterval (MkMyEvent2 x) =  getInterval x
  setInterval (MkMyEvent2 x) y = MkMyEvent2 $ setInterval x y

instance (Ord a, Eq c, Eq d) => EventModel2 MyEvent2 c d a

foo2 :: EventModel2 e c d a => e (Context c d) a -> [e (Context c d) a] -> [e (Context c d)  a]
foo2 x = filter (x ==)

foo2' :: (Eq a, Eq c, Eq d) => MyEvent2 (Context c d) a -> [ MyEvent2 (Context c d) a]  -> [ MyEvent2 (Context c d) a]
foo2' x = filter (x ==)
