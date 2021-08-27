{-|
Module      : Functions and types for creating tests for templates 
Description : Misc types and functions useful in Hasklepias.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

These functions may be moved to more appropriate modules in future versions.
-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Hasklepias.Templates.TestUtilities (
    TestCase(..)
  , evalTestCase
  , makeAssertion
  , readIntervalSafe
  , makeEnrollmentEvent
  , makeEventWithConcepts
) where

import Control.Applicative ( Applicative(pure) )
import Data.Bool ( Bool (True) )
import Data.Eq                          ( Eq )
import Data.Monoid ( Monoid(mempty) )
import Data.Text ( Text )
import Data.Tuple                       ( uncurry )
import           Data.Tuple.Curry
-- ( uncurryN )
import           GHC.Real                       ( Integral )

import GHC.Show                         ( Show )
import EventData
import Cohort.Index
import Features.Compose                 ( Feature
                                        , Definition(..)
                                        , Define(..)
                                        , Eval(..) )
import Hasklepias.Misc

import IntervalAlgebra
import Test.Tasty                       ( TestName )
import Test.Tasty.HUnit                 ( (@?=), Assertion )


data TestCase a b builderArgs = MkTestCase {
    getBuilderArgs :: builderArgs
  , getTestName :: TestName
  , getInputs :: a
  , getTruth  :: Feature "result" b
  } deriving (Eq, Show)


evalTestCase :: (Eval def defArgs return) =>
  TestCase defArgs b builderArgs
  -> Definition def
  -> ( return, Feature "result" b )
evalTestCase (MkTestCase buildArgs _ inputs truth) def = ( eval def inputs, truth )

makeAssertion :: (Eq b, Show b, Eval def defArgs (Feature "result" b)) =>
  TestCase defArgs b  builderArgs -> Definition def -> Assertion
makeAssertion x def = uncurry (@?=) (evalTestCase x def)

readIntervalSafe :: (Integral b, IntervalSizeable a b) => (a, a) -> Interval a
readIntervalSafe (b, e) = beginerval (diff e b) b

makeEnrollmentEvent :: (Integral b, IntervalSizeable a b) => (a, a) -> Event a
makeEnrollmentEvent intrvl =
  event (readIntervalSafe intrvl) (context (Enrollment (EnrollmentFacts ())) mempty)

makeEventWithConcepts :: (Integral b, IntervalSizeable a b) => [Text] -> (a, a) -> Event a
makeEventWithConcepts cpts intrvl = event
  (readIntervalSafe intrvl)
  (context (Enrollment (EnrollmentFacts ())) (packConcepts cpts))

makeTestTemplate
  :: (Integral b, IntervalSizeable a b)
  => TestName  -- ^ name of the test
  -> builderArgs -- ^ tuple of arguments pass to the definition builder
  -> (a, a)    -- ^ index interval 
  -> [Event a] -- ^ test events
  -> resultType -- ^ expected result
  -> TestCase
       (F "index" (Index Interval a), F "events" [Event a])
       resultType
       builderArgs
makeTestTemplate name buildArgs intrvl e b = MkTestCase
  buildArgs
  name
  (pure (makeIndex (readIntervalSafe intrvl) ), pure e)
  (pure b)
