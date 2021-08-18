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
    TemplateTestCase(..)
  , evalTestCase
  , makeAssertion
) where

import Data.Eq                          ( Eq )
import Data.Tuple                       ( uncurry )
import GHC.Show                         ( Show )
import Features.Compose                 ( Feature
                                        , Definition(..)
                                        , Define(..)
                                        , Eval(..) )
import Test.Tasty                       ( TestName )
import Test.Tasty.HUnit                 ( (@=?), Assertion )

data TemplateTestCase a b = MkTemplateTestCase {
    getTestName :: TestName
  , getInputs :: a
  , getTruth  :: Feature "result" b
  } deriving (Eq, Show)

evalTestCase :: Eval def args return => 
  TemplateTestCase args b 
  -> Definition def 
  -> ( return, Feature "result" b )
evalTestCase (MkTemplateTestCase _ inputs truth) def = ( eval def inputs, truth )

makeAssertion :: (Eq b, Show b, Eval def args (Feature "result" b)) =>
  TemplateTestCase args b -> Definition def -> Assertion
makeAssertion x def = uncurry (@=?) (evalTestCase x def)