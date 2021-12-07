{-| 
Module      : Functions and types for creating tests for templates 
Description : Misc types and functions useful in Hasklepias.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

These functions may be moved to more appropriate modules in future versions.
-}
-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Templates.TestUtilities
  ( TestCase(..)
  , readIntervalSafe
  , makeEnrollmentEvent
  , makeEventWithConcepts
  , makeTestCase
  , makeTestCaseOfIndexAndEvents
  , makeTestGroup
  , Solo
  ) where


import           Cohort.Index

-- #endif
-- TODO: find a better way to handle this import and/or figure out how use
--       gcc on a mac on the haskell c-preprocessor without phutzing with a lot
--       different settings.
-- #if MIN_VERSION_base(4,16,0)
-- import          GHC.Tuple (Solo (Solo), getSolo)
-- #elif MIN_VERSION_base(4,15,0)
--   import           GHC.Tuple (Solo (Solo))
-- #else
import           Data.Tuple.Solo

import           EventData
import           Features.Compose               ( Define(..)
                                                , Definition(..)
                                                , F
                                                , Feature
                                                , eval
                                                )
import           Hasklepias.Misc
import           Hasklepias.Reexports
import           Hasklepias.ReexportsUnsafe

{-
  a just few utilities for constructing intervals/events
-}
readIntervalSafe :: (Integral b, IntervalSizeable a b) => (a, a) -> Interval a
readIntervalSafe (b, e) = beginerval (diff e b) b

makeEnrollmentEvent :: (Integral b, IntervalSizeable a b) => (a, a) -> Event a
makeEnrollmentEvent intrvl = event
  (readIntervalSafe intrvl)
  (context (Enrollment emptyEnrollmentFact) mempty Nothing)

makeEventWithConcepts
  :: (Integral b, IntervalSizeable a b) => [Text] -> (a, a) -> Event a
makeEventWithConcepts cpts intrvl = event
  (readIntervalSafe intrvl)
  (context (Enrollment emptyEnrollmentFact) (packConcepts cpts) Nothing)

{-
  types/functions for creating test cases and evaluating them
-}

data TestCase a b builderArgs = MkTestCase
  { getBuilderArgs :: builderArgs
  , getTestName    :: TestName
  , getInputs      :: a
  , getTruth       :: Feature "result" b
  }
  deriving (Eq, Show)

evalTestCase
  :: TestCase defArgs b builderArgs
  -> (defArgs -> returnType)
  -> (returnType, Feature "result" b)
evalTestCase (MkTestCase buildArgs _ inputs truth) def = (def inputs, truth)

makeAssertion
  :: (Eq b, Show b)
  => TestCase defArgs b builderArgs
  -> (defArgs -> Feature "result" b)
  -> Assertion
makeAssertion x def = uncurry (@?=) (evalTestCase x def)

makeTestCase
  :: TestName
  -> bargs
  -> inputType
  -> returnType
  -> TestCase inputType returnType bargs
makeTestCase name buildArgs i b = MkTestCase buildArgs name i (pure b)

makeTestCaseOfIndexAndEvents
  :: (Integral b, IntervalSizeable a b)
  => TestName
  -> bargs
  -> (a, a)
  -> [Event a]
  -> returnType
  -> TestCase
       (F "index" (Index Interval a), F "events" [Event a])
       returnType
       bargs
makeTestCaseOfIndexAndEvents name buildArgs intrvl e = makeTestCase
  name
  buildArgs
  (pure (makeIndex (readIntervalSafe intrvl)), pure e)


makeBuilderAssertion
  :: ( Eq b1
     , Show b1
     , Curry (a -> Feature "result" b1) b2
     , Curry (t -> Definition b2) b3
     )
  => b3
  -> TestCase a b1 t
  -> Assertion
makeBuilderAssertion f x =
  makeAssertion x (uncurryN $ eval $ uncurryN f (getBuilderArgs x))

makeTestGroup
  :: ( Eq b1
     , Show b1
     , Curry (a -> Feature "result" b1) b2
     , Curry (t -> Definition b2) b3
     )
  => TestName
  -> b3
  -> [TestCase a b1 t]
  -> TestTree
makeTestGroup n f cases = testGroup
  n
  (fmap (\x -> testCase (getTestName x) (makeBuilderAssertion f x)) cases)
