{-|
Module      : Exports Hasklepias templates
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Hasklepias.Templates.Features (
     module Hasklepias.Templates.Features.Enrollment
   , module Hasklepias.Templates.Features.NsatisfyP
) where

import Hasklepias.Templates.Features.Enrollment
    ( buildIsEnrolled,
      buildContinuousEnrollment,
    )
import Hasklepias.Templates.Features.NsatisfyP 
   (  buildNofX
    , buildNofXBool
    , buildNofXBinary
    , buildNofXBinaryConcurBaseline
    , buildNofConceptsBinaryConcurBaseline
    , buildNofXWithGap
    , buildNofXWithGapBool
    , buildNofXWithGapBinary
    , buildNofUniqueBegins)