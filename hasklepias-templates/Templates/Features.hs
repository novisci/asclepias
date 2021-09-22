{-|
Module      : Exports Hasklepias templates
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Templates.Features (
     module Templates.Features.BuildIsEnrolled
   , module Templates.Features.Enrollment
   , module Templates.Features.NsatisfyP
) where

import Templates.Features.BuildIsEnrolled

import Templates.Features.Enrollment
    ( 
      -- buildIsEnrolled,
      buildContinuousEnrollment,
    )
import Templates.Features.NsatisfyP 
   (  buildNofX
    , buildNofXBool
    , buildNofXBinary
    , buildNofXBinaryConcurBaseline
    , buildNofConceptsBinaryConcurBaseline
    , buildNofXWithGap
    , buildNofXWithGapBool
    , buildNofXWithGapBinary
    , buildNofUniqueBegins
    , buildNofXOrNofYWithGap
    , buildNofXOrNofYWithGapBool
    , buildNofXOrNofYWithGapBinary )