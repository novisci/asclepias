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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}

module Hasklepias.Types.Feature(
    -- * Types
      FeatureSpec(..)
    , Feature(..)
    , FeatureData(..)
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
    , evalEFFeature
    , evalFEFFeature
    , evalFFFFeature
) where

import GHC.Read                   ( Read )
import GHC.Show                   ( Show )
import GHC.Generics               ( Generic )
import Data.Either                ( Either(..) )
import Data.Eq                    ( Eq )
import Data.Functor               ( Functor(fmap) )
import Data.Function              ( ($), (.) )
import Data.Maybe                 ( Maybe(..) )
import Data.Ord                   ( Ord )
import Data.Text                  ( Text )
import Hasklepias.Types.Event     ( Events )
import IntervalAlgebra            ( Interval, Intervallic )
-- import safe Test.QuickCheck       ( Property )

{- | A 'FeatureSpec' contains all the information needed to derive a 'Feature':
      * its name
      * its attributes
      * the function needed to derive a feature (i.e. the 'FeatureDefinition')
-}
data (Show b) => FeatureSpec b f e a d = FeatureSpec {
        getSpecName :: Text
      , getSpecAttr :: b
      , getDefn :: FeatureDefinition f e a d
      -- To add in future: an optional list of properties to check
      -- , getProp :: Maybe [Feature d -> Events a -> Property] 
    }

{- | A 'Feature' contains the following:
      * a name
      * its attributes
      * 'FeatureData'
-}
data (Show b) => Feature b d = Feature {
        getName :: Text
      , getAttr :: b
      , getData :: FeatureData d 
      }

{- | 'FeatureData' is @'Either' 'MissingReason' d@, where @d@ can be any type 
     of data derivable from 'Hasklepias.Event.Events'.
-}
newtype FeatureData d = FeatureData { getFeatureData :: Either MissingReason d }
  deriving (Generic, Show, Eq)

instance Functor FeatureData where
  fmap f (FeatureData x) = FeatureData (fmap f x)

-- | Create the 'Right' side of a 'Feature'.
featureR :: d -> FeatureData d
featureR = FeatureData . Right

-- | Create the 'Left' side of a 'Feature'.
featureL :: MissingReason -> FeatureData d
featureL = FeatureData . Left

-- | A 'Feature' may be missing for any number of reasons. 
data MissingReason =
    InsufficientData
  | Excluded
  | Other Text
  | Unknown
  deriving (Eq, Read, Show, Generic)

-- | A type to hold common FeatureData definitions; i.e. functions that return 
--  features.
data FeatureDefinition f e a d =
    EF  (Events a -> FeatureData d)
  | FEF (FeatureData e -> Events a -> FeatureData d)
  | FFF (FeatureData f -> FeatureData e -> FeatureData d)

-- | Define an 'EF' FeatureDefinition
defineEF ::  Ord a =>
             MissingReason 
          -- ^ The reason if @f@ returns 'Nothing' 
          -> (Events a -> Maybe c) 
          -- ^ A function that maps events to an some intermediary Maybe type. 
          --   In the case that this function returns 'Nothing', you get a 
          --   @Left@ FeatureData with the provided @MissingReason@. Otherwise, 
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
applyEF :: FeatureDefinition * * a d -> Events a -> FeatureData d
applyEF (EF f) = f

-- | TODO
defineFEF :: Ord a => 
             MissingReason
          -- ^ The reason if the input 'Feature' is a 'Left'.
          -> (e -> Events a -> d)
          -- ^ A function that tranforms the data of a 'Right' input 'Feature'
          --   and a collection of events into the desired type.
          -> FeatureDefinition * e a d
defineFEF r g = FEF (\(FeatureData feat) es ->
  case feat of
    (Left _)  -> featureL r
    (Right x) -> featureR (g x es)
  )

-- | TODO
defineFEF2 :: Ord a =>
             MissingReason
          -- ^ The reason if the input 'Feature' is a 'Left'.
          -> (e -> Events a -> FeatureData d)
          -- ^ A function that tranforms the data of a 'Right' input 'Feature'
          --   and a collection of events into the desired type.
          -> FeatureDefinition * e a d
defineFEF2 r g = FEF (\(FeatureData feat) es ->
  case feat of
    (Left _)  -> featureL r
    (Right x) -> g x es
  )

-- | Extract a 'FEF' FeatureDefinition
applyFEF :: FeatureDefinition * e a d -> FeatureData e -> Events a -> FeatureData d
applyFEF (FEF f) = f

-- | TODO
defineFFF :: 
        MissingReason
    ->  MissingReason      
    -> (f -> e -> d) 
    -> FeatureDefinition f e * d
defineFFF r1 r2 g = FFF (\(FeatureData feat1) (FeatureData feat2) ->
    case ( feat1, feat2 ) of 
      ( Left _ , Left _ ) -> featureL r1
      ( Left _ , _      ) -> featureL r1
      ( _      , Left _ ) -> featureL r2
      ( Right x, Right y) -> featureR $ g x y
  )

-- | Extract a 'FFF' FeatureDefinition
applyFFF :: FeatureDefinition f e * d -> FeatureData f -> FeatureData e -> FeatureData d
applyFFF (FFF f) = f

-- | TODO
makeFeatureSpec :: Show b => Text -> b -> FeatureDefinition f e a d ->  
  FeatureSpec b f e a d
makeFeatureSpec = FeatureSpec

-- | TODO
evalEFFeature :: Show b => FeatureSpec b * * a d -> Events a -> Feature b d 
evalEFFeature (FeatureSpec n atr def) es = 
    Feature n atr (applyEF def es)

-- | TODO 
evalFEFFeature :: Show b => FeatureSpec b * e a d -> Feature b e -> Events a -> Feature b d 
evalFEFFeature (FeatureSpec n atr def) feat es =
    Feature n atr (applyFEF def (getData feat) es)

-- | TODO
evalFFFFeature :: Show b => FeatureSpec b f e * d -> Feature b f -> Feature b e -> Feature b d 
evalFFFFeature (FeatureSpec n atr def) feat1 feat2 =
    Feature n atr (applyFFF def (getData feat1) (getData feat2))