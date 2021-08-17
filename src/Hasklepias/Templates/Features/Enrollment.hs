{-|
Module      : Misc types and functions 
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

module Hasklepias.Templates.Features.Enrollment (
  defContinuousEnrollment
) where

import Features.Compose (Feature, Definition(..), Define(..))
import GHC.TypeLits ( KnownSymbol )
import Cohort.Index ( Index(..) )
import Cohort.Criteria ( Status(..), includeIf )
import IntervalAlgebra
import IntervalAlgebra.PairedInterval
import Hasklepias.FeatureEvents   ( allGapsWithinLessThanDuration
                                  , makeConceptsFilter )
import Witherable ( Witherable )
import Data.Foldable (Foldable(..), any)
import Data.Function ( ($), (.) )
import Data.Maybe ( Maybe )
import Data.Monoid ( Monoid )
import Data.Text ( Text )
import EventData.Core ( Event )
import Control.Applicative ( Applicative )
import Flow

{-|
-}

-- | Include the subject if she has an enrollment interval concurring with index.
defIsEnrolled :: 
  ( KnownSymbol indexName
  , Intervallic i0 a
  , Foldable f) =>
  Definition
  (   Feature indexName (Index i0 a)
   -> Feature "enrollmentIntervals" (f (Event a))
   -> Feature "isEnrolled" Status )
defIsEnrolled =
  define
      (\index events ->
        events
        |> any (concur $ getIndex index)
        |> includeIf
      )

{-| Continuous Enrollment 

TODO: describe this
-}
defContinuousEnrollment ::
  ( KnownSymbol indexName
  , KnownSymbol eventsName
  , KnownSymbol prevName
  , KnownSymbol varName
  , Monoid (container (Interval a))
  , Monoid (container (Maybe (Interval a)))
  , Applicative container
  , Witherable container
  , IntervalCombinable i1 a
  , IntervalSizeable a b) =>
    (Index i0 a -> i1 a) -- ^ function which maps index interval to interval in which to assess enrollment
  -> b                   -- ^ duration of allowable gap between enrollment intervals
  -> [Text]              -- ^ list of concepts which identify enrollment
  -> Definition
  (   Feature indexName  (Index i0 a) 
   -> Feature eventsName (container (Event a))
   -> Feature prevName    Status  
   -> Feature varName     Status )
defContinuousEnrollment formInterval allowableGap concepts =
  define
    (\index events prevStatus ->
      case prevStatus of
        Exclude -> Exclude
        Include -> includeIf
          ( allGapsWithinLessThanDuration
                allowableGap
                (formInterval index)
                (intervals $ makeConceptsFilter concepts events))
    )
