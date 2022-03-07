{-|
Module      : Functions and types for creating tests for templates 
Description : Misc types and functions useful in Hasklepias.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

These functions may be moved to more appropriate modules in future versions.
-}
-- {-# OPTIONS_HADDOCK hide #-}

module Templates.Tests
  ( testsMain
  ) where


import           Templates.Features.BuildContinuousEnrollment
                                                ( buildContinuousEnrollmentTests
                                                )
import           Templates.Features.BuildIsEnrolled
                                                ( buildIsEnrolledTests )
import           Templates.Features.BuildNofUniqueBegins
                                                ( buildNofUniqueBeginsTests )
import           Templates.Features.BuildNofX   ( buildNofXTests )
import           Templates.Features.BuildNofXOrMofYWithGap
                                                ( buildNofXOrMofYWithGapTests )
import           Templates.Features.BuildNofXWithGap
                                                ( buildNofXWithGapTests )
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )

templateTests :: TestTree
templateTests = testGroup
  "Tests of feature building templates"
  [ buildIsEnrolledTests
  , buildContinuousEnrollmentTests
  , buildNofXTests
  , buildNofXWithGapTests
  , buildNofXOrMofYWithGapTests
  , buildNofUniqueBeginsTests
  ]

testsMain :: IO ()
testsMain = defaultMain templateTests
