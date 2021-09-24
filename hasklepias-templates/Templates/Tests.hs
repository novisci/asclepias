{-|
Module      : Functions and types for creating tests for templates 
Description : Misc types and functions useful in Hasklepias.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

These functions may be moved to more appropriate modules in future versions.
-}
-- {-# OPTIONS_HADDOCK hide #-}

module Templates.Tests (
   templateTests
) where


import Test.Tasty                                    ( testGroup, TestTree )  
import Templates.Features.BuildIsEnrolled            ( buildIsEnrolledTests )                     
import Templates.Features.BuildContinuousEnrollment  ( buildContinuousEnrollmentTests )
import Templates.Features.BuildNofX                  ( buildNofXTests )
import Templates.Features.BuildNofXWithGap           ( buildNofXWithGapTests )
import Templates.Features.BuildNofXOrNofYWithGap     ( buildNofXOrNofYWithGapTests )
import Templates.Features.BuildNofUniqueBegins       ( buildNofUniqueBeginsTests )

templateTests :: TestTree
templateTests = 
   testGroup 
      "Tests of feature building templates" 
      [ buildIsEnrolledTests 
      , buildContinuousEnrollmentTests
      , buildNofXTests
      , buildNofXWithGapTests
      , buildNofXOrNofYWithGapTests
      , buildNofUniqueBeginsTests
      ]
