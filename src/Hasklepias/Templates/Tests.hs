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

module Hasklepias.Templates.Tests (
   templateTests
) where

import Test.Tasty                               ( testGroup, TestTree )                       
import Hasklepias.Templates.Features.Enrollment ( defEnrollmentTests )
import Hasklepias.Templates.Features.NsatisfyP  ( defNsatisfyPTests )

templateTests :: TestTree
templateTests = testGroup "tests of feature templates" 
   [ defEnrollmentTests
   , defNsatisfyPTests ]
