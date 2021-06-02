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
module EventData.Arbitrary(
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
import GHC.Show ( Show )
import GHC.IO ( IO )
import Control.Monad ( Functor(fmap), liftM2 )
import Data.Eq ( Eq((==)) )
import Data.Function (($))
import Data.Int ( Int )
import Data.Ord ( Ord )
import Data.List(length)
import IntervalAlgebra ( Interval )
import IntervalAlgebra.Arbitrary ()
import EventData
    ( event, Event, ConceptEvent, toConceptEvent )
import EventData.Context.Arbitrary ()

instance (Arbitrary (Interval a)) => Arbitrary (Event a) where
    arbitrary = liftM2 event arbitrary arbitrary

instance (Ord a, Show a, Arbitrary (Interval a)) => Arbitrary (ConceptEvent a) where
    arbitrary = fmap toConceptEvent arbitrary

-- | Generate @n@ @Event Int@
generateEventsInt :: Int -> IO [Event Int]
generateEventsInt i = 
    generate $ suchThat (orderedList :: Gen [Event Int]) (\x -> length x == i)