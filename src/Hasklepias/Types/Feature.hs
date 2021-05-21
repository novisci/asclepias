{-|
Module      : Hasklepias Feature Type
Description : Defines the Feature type and its component types, constructors, 
              and class instances
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE Safe #-}

module Hasklepias.Types.Feature(
    -- * Types
      Feature(..)
    , MissingReason(..)
    , FeatureDefinition(..)
    , defineEF
    , defineFEF
    , defineFEF2
    , defineFFF
    , applyEF
    , applyFEF
    , applyFFF
    , featureR
    , featureL

) where

import GHC.Read                   ( Read )
import GHC.Show                   ( Show )
import GHC.Generics               ( Generic )
import Data.Either                ( Either(..) )
import Data.Eq                    ( Eq )
import Data.Functor               ( Functor(fmap) )
import Data.Function              ( ($), (.) )
import Data.Maybe                 ( Maybe(..) )
import Data.String                ( String )
import Hasklepias.Types.Event     ( Events )
import IntervalAlgebra            ( Interval, Intervallic )

{- | A 'Feature' is a @'Either' 'MissingReason' d@, where @d@ can be any type 
     of data derivable from 'Hasklepias.Event.Events'.
-}
newtype Feature d = Feature { getFeature :: Either MissingReason d }
  deriving (Generic, Show, Eq)

instance Functor Feature where
  fmap f (Feature x) = Feature (fmap f x)

-- | Create the 'Right' side of a 'Feature'.
featureR :: d -> Feature d
featureR = Feature . Right

-- | Create the 'Left' side of a 'Feature'.
featureL :: MissingReason -> Feature d
featureL = Feature . Left

-- | A 'Feature' may be missing for any number of reasons. 
data MissingReason =
    InsufficientData
  | Excluded
  | Other String
  | Unknown
  deriving (Eq, Read, Show, Generic)

-- | A type to hold common feature definitions; i.e. functions that return 
--  features.
data FeatureDefinition f e a d =
    EF  (Events a -> Feature d)
  | FEF (Feature e -> Events a -> Feature d)
  | FFF (Feature f -> Feature e -> Feature d)

-- | Define an 'EF' FeatureDefinition
defineEF :: (Intervallic Interval a) =>
             MissingReason 
          -- ^ The reason if @f@ returns 'Nothing' 
          -> (Events a -> Maybe c) 
          -- ^ A function that maps events to an some intermediary Maybe type. 
          --   In the case that this function returns 'Nothing', you get a 
          --   @Left@ feature with the provided @MissingReason@. Otherwise, 
          --   the 'Just' result is passed to the next function for final
          --   transformation to the desired @Feature@ type.
          -> (c -> d)              
          -- ^ A function that transforms the intermediary data to the desired 
          --   type.
          -> FeatureDefinition * e a d
defineEF r f g = EF (\es ->
  case f es of
    Nothing -> featureL r
    Just x  -> featureR (g x)
  )

-- | Extract an 'EF' FeatureDefinition.
applyEF :: FeatureDefinition * * a d -> Events a -> Feature d
applyEF (EF f) = f

-- | TODO
defineFEF :: (Intervallic Interval a) =>
             MissingReason
          -- ^ The reason if the input 'Feature' is a 'Left'.
          -> (e -> Events a -> d)
          -- ^ A function that tranforms the data of a 'Right' input 'Feature'
          --   and a collection of events into the desired type.
          -> FeatureDefinition * e a d
defineFEF r g = FEF (\(Feature feat) es ->
  case feat of
    (Left _)  -> featureL r
    (Right x) -> featureR (g x es)
  )

-- | TODO
defineFEF2 :: (Intervallic Interval a) =>
             MissingReason
          -- ^ The reason if the input 'Feature' is a 'Left'.
          -> (e -> Events a -> Feature d)
          -- ^ A function that tranforms the data of a 'Right' input 'Feature'
          --   and a collection of events into the desired type.
          -> FeatureDefinition * e a d
defineFEF2 r g = FEF (\(Feature feat) es ->
  case feat of
    (Left _)  -> featureL r
    (Right x) -> g x es
  )

-- | Extract a 'FEF' FeatureDefinition
applyFEF :: FeatureDefinition * e a d -> Feature e -> Events a -> Feature d
applyFEF (FEF f) = f

-- | TODO
defineFFF :: 
        MissingReason
    ->  MissingReason      
    -> (f -> e -> d) 
    -> FeatureDefinition f e * d
defineFFF r1 r2 g = FFF (\(Feature feat1) (Feature feat2) ->
    case ( feat1, feat2 ) of 
      ( Left _ , Left _ ) -> featureL r1
      ( Left _ , _      ) -> featureL r1
      ( _      , Left _ ) -> featureL r2
      ( Right x, Right y) -> featureR $ g x y
  )

-- | Extract a 'FFF' FeatureDefinition
applyFFF :: FeatureDefinition f e * d -> Feature f -> Feature e -> Feature d
applyFFF (FFF f) = f