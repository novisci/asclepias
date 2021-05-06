{-|
Module      : Generate arbitrary events
Description : Functions for generating arbitrary events
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Hasklepias.Types.Event.Arbitrary(
     generateEventsInt
) where

import Test.QuickCheck (
    Arbitrary(arbitrary, shrink)
    , Gen
    , sample'
    , sample
    , generate
    , resize
    , suchThat 
    , orderedList  )
import GHC.Base(Int, Ord, liftM2, fmap, IO, ($), (==) )
import Data.List(length)
import IntervalAlgebra ( Interval )
import IntervalAlgebra.Arbitrary ()
import Hasklepias.Types.Event
    ( event, Event, ConceptEvent, toConceptEvent )
import Hasklepias.Types.Context.Arbitrary ()

instance (Arbitrary (Interval a)) => Arbitrary (Event a) where
    arbitrary = liftM2 event arbitrary arbitrary

instance (Ord a, Arbitrary (Interval a)) => Arbitrary (ConceptEvent a) where
    arbitrary = fmap toConceptEvent arbitrary

-- | Generate @n@ @Event Int@
generateEventsInt :: Int -> IO [Event Int]
generateEventsInt i = generate $ suchThat (orderedList :: Gen [Event Int]) (\x -> length x == i)
