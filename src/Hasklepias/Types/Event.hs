{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : Hasklepias Event Type
Description : Defines the Event type and its component types, constructors, 
              and class instance
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

module Hasklepias.Types.Event(
   Event(getEvent)
 , Events
 , event
 , intrvl
 , ctxt
) where

import GHC.Base(Eq, Ord(..), (++), ($), not, (.))
import GHC.Show ( Show(show) )
import Data.Tuple ( fst, snd )
import IntervalAlgebra( Interval, IntervalAlgebraic, Intervallic )
import Hasklepias.Types.Context ( HasConcept(..), Context )

-- | An Event @a@ is simply a pair @(Interval a, Context)@.
newtype Event a =  Event { getEvent :: (Interval a, Context) }
  deriving (Eq)

instance (Intervallic a) => Ord (Event a) where
  (<=) (Event x) (Event y) = fst x <= fst y
  (<)  (Event x) (Event y) = fst x <  fst y
  (>=) x y = not (x < y)
  (>)  x y = not (x <= y)

instance (Intervallic a, Show a) => Show (Event a) where
  show x = "{" ++ show (fst $ getEvent x) ++ ", " ++ show (snd $ getEvent x) ++ "}"

instance HasConcept (Event a) where
    hasConcept x y = snd (getEvent x) `hasConcept` y

-- | A smart constructor for 'Event a's.
event :: (IntervalAlgebraic a) => Interval a -> Context -> Event a
event i c = Event (i, c)

-- | Access the 'Interval a' of an 'Event a'.
intrvl :: Event a -> Interval a
intrvl = fst.getEvent

-- | Access the 'Context' of an 'Event a'.
ctxt :: Event a -> Context
ctxt = snd.getEvent

-- | A @List@ of @Event a@
-- 
-- NOTE (20190911): I (B. Saul) am starting out the Events type as a 
-- list of the Event type. This may be not be the optimal approach,
-- especially with regards to lookup/filtering the list. Ideally,
-- we could do one pass through the ordered container (whatever it is)
-- to identify events by concept; rather than repeated evaluations of
-- the lookup predicates. This could be handled by, for example, 
-- representing Events has a Map with a list of concept indices. 
-- But this gets us off the ground.
type Events a = [Event a]
