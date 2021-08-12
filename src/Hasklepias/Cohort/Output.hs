{-|
Module      : Hasklepias Cohorts
Description : Defines the options for outputting a cohort
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hasklepias.Cohort.Output(
    CohortShape
  , ShapeCohort(..)
  , toJSONCohortShape
) where

import Data.Aeson                           ( ToJSON(..)
                                            , Value
                                            , object
                                            , (.=) )
import Data.Function                        ( (.) )
import Data.Functor                         ( Functor(fmap) )
import Data.List.NonEmpty as NE             ( NonEmpty(..)
                                            , head
                                            , fromList
                                            , zip )
import Data.Tuple                           ( uncurry )
import GHC.Generics                         ( Generic )
import GHC.Types                            ( Type )
import GHC.Show                             ( Show )
import Hasklepias.Cohort.Core               ( AttritionInfo,
                                              Cohort,
                                              ObsUnit,
                                              ID,
                                              CohortData,
                                              getCohortData,
                                              getCohortIDs )
import Hasklepias.Cohort.Criteria           ( CohortStatus )
import Features.Featureset                  ( FeaturesetList(MkFeaturesetList)
                                            , Featureset
                                            , getFeatureset
                                            , getFeaturesetList
                                            , tpose )
import Features.Output                      ( ShapeOutput(dataOnly, nameAttr)
                                            , OutputShape )

instance (ToJSON d) => ToJSON (ObsUnit d) where
instance (ToJSON d) => ToJSON (CohortData d) where
instance (ToJSON d) => ToJSON (Cohort d) where
instance ToJSON CohortStatus where
instance ToJSON AttritionInfo where

-- | A type used to determine the output shape of a Cohort.
data CohortShape d where
  ColumnWise :: (Show a, ToJSON a) => a -> CohortShape ColumnWise
  RowWise :: (Show a, ToJSON a) => a -> CohortShape RowWise

deriving instance Show d => Show (CohortShape d)

-- | Maps CohortShape into an Aeson Value. 
-- TODO: implement Generic and ToJSON instance of CohortShape directly.
toJSONCohortShape :: CohortShape shape -> Value
toJSONCohortShape (ColumnWise x) = toJSON x
toJSONCohortShape (RowWise x)    = toJSON x

class ShapeCohort d where
  colWise :: Cohort d -> CohortShape ColumnWise
  rowWise :: Cohort d -> CohortShape RowWise

instance ShapeCohort Featureset where
  colWise x = ColumnWise (shapeColumnWise x)
  rowWise x = RowWise (shapeRowWise x)

data ColumnWise = MkColumnWise {
    colAttributes :: NonEmpty (OutputShape Type)
  , ids     :: [ID]
  , colData :: NonEmpty (NonEmpty (OutputShape Type))
  } deriving ( Show, Generic )

instance ToJSON ColumnWise where
  toJSON x = object [ "attributes" .= colAttributes x
                    , "ids"        .= ids x
                    , "data"       .= colData x ]

newtype IDRow = MkIDRow (ID, NonEmpty (OutputShape Type))
  deriving ( Show, Generic )

instance ToJSON IDRow where
  toJSON (MkIDRow x) = object [ uncurry (.=) x]

data RowWise = MkRowWise {
    attributes :: NonEmpty (OutputShape Type)
  , rowData :: NonEmpty IDRow
  } deriving ( Show, Generic )

instance ToJSON RowWise where
  toJSON x = object [ "attributes" .= attributes x
                    , "data"       .= rowData x ]

shapeColumnWise :: Cohort Featureset -> ColumnWise
shapeColumnWise x = MkColumnWise
        (fmap (nameAttr . NE.head . getFeatureset) z)
        (getCohortIDs x)
        (fmap (fmap dataOnly . getFeatureset)  z)
        -- TODO: don't use fromList; do something more principled
        where z = getFeaturesetList (tpose (MkFeaturesetList (NE.fromList (getCohortData x))))

shapeRowWise :: Cohort Featureset -> RowWise
shapeRowWise x = MkRowWise
        (fmap (nameAttr . NE.head . getFeatureset) z)
        (fmap MkIDRow (zip ids (fmap (fmap dataOnly . getFeatureset) z)))
        -- TODO: don't use fromList; do something more principled
        where z = NE.fromList (getCohortData x)
              ids = fromList (getCohortIDs x)