{-|
Module      : Generate arbitrary events 
Description : Functions for generating arbitrary events
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}
-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module EventData.Arbitrary
  (
   -- generateEventsInt
  ) where

import           Control.Monad                  ( Functor(fmap)
                                                , liftM2
                                                )
import           Data.Eq                        ( Eq((==)) )
import           Data.Function                  ( ($) )
import           Data.Int                       ( Int )
import           Data.List                      ( length )
import           Data.Ord                       ( Ord )
import           EventData.Context.Arbitrary    ( )
import           EventData.Core                 ( ConceptEvent
                                                , Event
                                                , event
                                                , toConceptEvent
                                                )
import           GHC.IO                         ( IO )
import           GHC.Show                       ( Show )
import           IntervalAlgebra                ( Interval )
import           IntervalAlgebra.Arbitrary      ( )
import           Test.QuickCheck                ( Arbitrary(arbitrary, shrink)
                                                , Gen
                                                , generate
                                                , orderedList
                                                , resize
                                                , sample
                                                , sample'
                                                , suchThat
                                                )

{- !!! Temporarily Disabled 
  - will be moved to event-data-theory
  - update to interval algebra 1.2 created overlapping instances errors
   

instance (Arbitrary (Interval a)) => Arbitrary (Event a) where
  arbitrary = liftM2 event arbitrary arbitrary

instance (Ord a, Show a, Arbitrary (Interval a)) => Arbitrary (ConceptEvent a) where
  arbitrary = fmap toConceptEvent arbitrary

-- | Generate @n@ @Event Int@
generateEventsInt :: Int -> IO [Event Int]
generateEventsInt i =
  generate $ suchThat (orderedList :: Gen [Event Int]) (\x -> length x == i)
-}
