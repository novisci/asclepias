{-|
Module      : Hasklepias Event Type
Description : Defines the Event type and its component types, constructors, 
              and class instance
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hasklepias.Types.Event(
   Event
 , Events
 , event
 , ctxt
) where

import GHC.Base(Eq, Ord(..), (++), ($), not, (.))
import GHC.Show ( Show(show) )
import IntervalAlgebra( Interval, IntervalAlgebraic, Intervallic (getInterval) )
import IntervalAlgebra.PairedInterval
    ( mkPairedInterval, pairData, PairedInterval )
import Hasklepias.Types.Context ( HasConcept(..), Context )

-- | An Event @a@ is simply a pair @(Interval a, Context)@.
type Event a = PairedInterval Context a

instance (Ord a, Show a) => Show (Event a) where
  show x = "{" ++ show (getInterval x) ++ ", " ++ show (ctxt x) ++ "}"

instance HasConcept (Event a) where
    hasConcept x y = ctxt x `hasConcept` y

-- | A smart constructor for 'Event a's.
event :: Interval a -> Context -> Event a
event i c = mkPairedInterval c i

-- | Access the 'Context' of an 'Event a'.
ctxt :: Event a -> Context
ctxt = pairData

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
