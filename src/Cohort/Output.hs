{-|
Module      : Cohort output (and some input)
Description : Methods for outputting a cohort
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

module Cohort.Output
  ( CohortShape
  , ShapeCohort(..)
  , toJSONCohortShape
  ) where

import           Cohort.Core                    ( AttritionInfo
                                                , AttritionLevel
                                                , Cohort
                                                , CohortData
                                                , ID
                                                , ObsUnit
                                                , getCohortData
                                                , getCohortIDs
                                                )
import           Cohort.Criteria                ( CohortStatus )
import           Data.Aeson                     ( (.=)
                                                , FromJSON
                                                , ToJSON(..)
                                                , Value
                                                , object
                                                )
import           Data.Function                  ( (.) )
import           Data.Functor                   ( Functor(fmap) )
import           Data.List.NonEmpty            as NE
                                                ( NonEmpty(..)
                                                , fromList
                                                , head
                                                , zip
                                                )
import           Data.Tuple                     ( uncurry )
import           Features.Featureset            ( Featureset
                                                , FeaturesetList
                                                  ( MkFeaturesetList
                                                  )
                                                , getFeatureset
                                                , getFeaturesetList
                                                , tpose
                                                )
import           Features.Output                ( OutputShape
                                                , ShapeOutput
                                                  ( dataOnly
                                                  , nameAttr
                                                  )
                                                )
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show )
import           GHC.Types                      ( Type )

instance (ToJSON d) => ToJSON (ObsUnit d) where
instance (ToJSON d) => ToJSON (CohortData d) where
instance (ToJSON d) => ToJSON (Cohort d) where

-- NOTE: The following purposefully use default encodings to make roundtrip easier
--       They can be changed from the default, but be sure that one can go to/from
--       JSON.
instance ToJSON CohortStatus where
instance FromJSON CohortStatus where
instance ToJSON AttritionLevel where
instance FromJSON AttritionLevel where
instance ToJSON AttritionInfo where
instance FromJSON AttritionInfo where

-- | A type used to determine the output shape of a Cohort.
data CohortShape d where
  ColumnWise ::(Show a, ToJSON a) => a -> CohortShape ColumnWise
  RowWise ::(Show a, ToJSON a) => a -> CohortShape RowWise

deriving instance Show d => Show (CohortShape d)

-- | Maps CohortShape into an Aeson Value. 
-- TODO: implement Generic and ToJSON instance of CohortShape directly.
toJSONCohortShape :: CohortShape shape -> Value
toJSONCohortShape (ColumnWise x) = toJSON x
toJSONCohortShape (RowWise    x) = toJSON x

-- | Provides methods for reshaping a 'Cohort.Cohort' to a 'CohortShape'.
class ShapeCohort d where
  colWise :: Cohort d -> CohortShape ColumnWise
  rowWise :: Cohort d -> CohortShape RowWise

instance ShapeCohort Featureset where
  colWise x = ColumnWise (shapeColumnWise x)
  rowWise x = RowWise (shapeRowWise x)

data ColumnWise = MkColumnWise
  { colAttributes :: NonEmpty (OutputShape Type)
  , ids           :: [ID]
  , colData       :: NonEmpty (NonEmpty (OutputShape Type))
  }
  deriving (Show, Generic)

instance ToJSON ColumnWise where
  toJSON x = object
    ["attributes" .= colAttributes x, "ids" .= ids x, "data" .= colData x]

newtype IDRow = MkIDRow (ID, NonEmpty (OutputShape Type))
  deriving ( Show, Generic )

instance ToJSON IDRow where
  toJSON (MkIDRow x) = object [uncurry (.=) x]

data RowWise = MkRowWise
  { attributes :: NonEmpty (OutputShape Type)
  , rowData    :: NonEmpty IDRow
  }
  deriving (Show, Generic)

instance ToJSON RowWise where
  toJSON x = object ["attributes" .= attributes x, "data" .= rowData x]

shapeColumnWise :: Cohort Featureset -> ColumnWise
shapeColumnWise x = MkColumnWise
  (fmap (nameAttr . NE.head . getFeatureset) z)
  (getCohortIDs x)
  (fmap (fmap dataOnly . getFeatureset) z)
        -- TODO: don't use fromList; do something more principled
 where
  z =
    getFeaturesetList (tpose (MkFeaturesetList (NE.fromList (getCohortData x))))

shapeRowWise :: Cohort Featureset -> RowWise
shapeRowWise x = MkRowWise
  (fmap (nameAttr . NE.head . getFeatureset) z)
  (fmap MkIDRow (zip ids (fmap (fmap dataOnly . getFeatureset) z)))
        -- TODO: don't use fromList; do something more principled
 where
  z   = NE.fromList (getCohortData x)
  ids = fromList (getCohortIDs x)
