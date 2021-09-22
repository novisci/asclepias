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

module Templates.Tests (
   templateTests
) where


import Test.Tasty                               ( testGroup, TestTree )                       
import Templates.Features.Enrollment ( buildEnrollmentTests )
import Templates.Features.NsatisfyP  ( buildNsatisfyPTests )

templateTests :: TestTree
templateTests = 
   testGroup 
      "tests of feature templates" 
      [ buildEnrollmentTests
      , buildNsatisfyPTests ]
