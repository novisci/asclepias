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
{-# LANGUAGE MultiParamTypeClasses #-}

module Cohort.Output
  ( CohortJSON
  , CohortSetJSON(..)
  , CohortDataShape
  , CohortDataShapeJSON(..)
  , ColumnWiseJSON(..)
  , RowWiseJSON(..)
  , ShapeCohort(..)
  , toJSONCohortDataShape
  ) where

import           Cohort.Core                    ( AttritionInfo
                                                , AttritionLevel
                                                , Cohort(..)
                                                , CohortData
                                                , CohortSet(..)
                                                , ID
                                                , ObsUnit
                                                , getCohortData
                                                , getCohortDataData
                                                , getCohortDataIDs
                                                , getCohortIDs
                                                )
import           Cohort.Criteria                ( CohortStatus )
import           Data.Aeson                     ( (.=)
                                                , FromJSON
                                                , ToJSON(..)
                                                , Value
                                                , object
                                                )
import           Data.Aeson.Types               ( FromJSON )
import           Data.Eq                        ( Eq )
import           Data.Function                  ( ($)
                                                , (.)
                                                )
import           Data.Functor                   ( Functor(fmap) )
import           Data.List                      ( zip, zipWith )
import           Data.List.NonEmpty            as NE
                                                ( NonEmpty(..)
                                                , nonEmpty
                                                , head
                                                , toList
                                                )
import           Data.Map.Strict               as Data.Map
                                                ( Map
                                                , unionWith )
import           Data.Maybe                     ( maybe, maybeToList, fromMaybe )
import           Data.Semigroup                 ( Semigroup(..) )
import           Data.Text                      ( Text )
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
instance (ToJSON d) => ToJSON (CohortSet d)

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
data CohortDataShape d where
  ColumnWise :: (Show a, ToJSON a) => a -> CohortDataShape ColumnWise
  RowWise :: (Show a, ToJSON a) => a -> CohortDataShape RowWise

deriving instance Show d => Show (CohortDataShape d)

-- TODO: implement Generic and ToJSON instance of CohortDataShape directly.
-- | Maps CohortDataShape into an Aeson Value. 
toJSONCohortDataShape :: CohortDataShape shape -> Value
toJSONCohortDataShape (ColumnWise x) = toJSON x
toJSONCohortDataShape (RowWise    x) = toJSON x

{- | 
A type containing all the information of a 'Cohort' but where the 'CohortData'
has been reshaped to a 'CohortDataShapeJSON'.
-}
newtype CohortJSON = MkCohortJSON (AttritionInfo, CohortDataShapeJSON)
    deriving (Eq, Show, Generic)

instance ToJSON CohortJSON
instance FromJSON CohortJSON

instance Semigroup CohortJSON where
  (<>) (MkCohortJSON x) (MkCohortJSON y) = MkCohortJSON (x <> y)

{- | 
Similar to 'CohortSet', but where the 'Cohort's have been mapped to a 'CohortJSON'.
-}
newtype CohortSetJSON = MkCohortSetJSON (Map Text CohortJSON)
    deriving (Eq, Show, Generic)

instance ToJSON CohortSetJSON
instance FromJSON CohortSetJSON

instance Semigroup CohortSetJSON where
  (<>) (MkCohortSetJSON x) (MkCohortSetJSON y) = 
    MkCohortSetJSON (unionWith (<>) x y)

-- | A type used to represent JSON formats for each shape
data CohortDataShapeJSON =
    CW ColumnWiseJSON
  | RW RowWiseJSON
  deriving (Eq, Show, Generic)

instance ToJSON    CohortDataShapeJSON
instance FromJSON  CohortDataShapeJSON
instance Semigroup CohortDataShapeJSON where
  (<>) (CW x) (CW y) = CW (x <> y)
  (<>) (RW x) (RW y) = RW (x <> y)
  (<>) (RW x) (CW y) = RW x
  (<>) (CW x) (RW y) = CW x

-- | Provides methods for reshaping a 'Cohort.Cohort' to a 'CohortDataShapeJSON'.
class ShapeCohort d where
  colWise :: Cohort d -> CohortJSON
  rowWise :: Cohort d -> CohortJSON

instance ShapeCohort Featureset  where
  colWise (MkCohort (a, d)) = MkCohortJSON (a, CW $ colWiseJson (shapeColumnWise d))
  rowWise (MkCohort (a, d)) = MkCohortJSON (a, RW $ rowWiseJson (shapeRowWise d))

---- ColumnWise ---- 

data ColumnWise = MkColumnWise [OutputShape Type] -- attributes
                                                  [ID] -- ids
                                                       [[OutputShape Type]] -- data
  deriving (Show, Generic)

instance ToJSON ColumnWise where

-- | A type to hold 'Cohort' information in a column-wise manner.
data ColumnWiseJSON = MkColumnWiseJSON
  { colAttributes :: [Value]
  , ids           :: [Value]
  , colData       :: [[Value]]
  }
  deriving (Eq, Show, Generic)

instance ToJSON   ColumnWiseJSON
instance FromJSON ColumnWiseJSON

instance Semigroup ColumnWiseJSON where
  (<>) (MkColumnWiseJSON a1 i1 d1) (MkColumnWiseJSON _ i2 d2) =
    MkColumnWiseJSON a1 (i1 <> i2) (zipWith (<>) d1 d2)

colWiseJson :: ColumnWise -> ColumnWiseJSON
colWiseJson (MkColumnWise a ids cd) =
  MkColumnWiseJSON (fmap toJSON a) (fmap toJSON ids) (fmap (fmap toJSON) cd)

shapeColumnWise :: CohortData Featureset -> ColumnWise
shapeColumnWise x = MkColumnWise
  (fromMaybe [] attr)
  (getCohortDataIDs x)
  (fromMaybe [[]] dat)
 where
  feat = fmap (getFeaturesetList . (tpose . MkFeaturesetList)) (nonEmpty (getCohortDataData x))
  attr = fmap (toList . fmap (nameAttr . NE.head . getFeatureset)) feat
  dat  = fmap (toList . fmap (toList . (fmap dataOnly . getFeatureset))) feat

---- Rowwise ---- 

newtype IDRow = MkIDRow (ID, [OutputShape Type])
  deriving ( Show, Generic )

instance ToJSON IDRow where
  -- toJSON (MkIDRow x) = object [uncurry (.=) x]

data RowWise = MkRowWise [OutputShape Type] -- attributes
                                            [IDRow]  -- data
  deriving (Show, Generic)

instance ToJSON RowWise where

-- | A type to hold 'Cohort' information in a row-wise manner.
data RowWiseJSON = MkRowWiseJSON
  { rowAttributes :: [Value]
  , rowData    :: [Value]
  }
  deriving (Eq, Show, Generic)

instance ToJSON   RowWiseJSON
instance FromJSON RowWiseJSON
instance Semigroup RowWiseJSON where
  (<>) (MkRowWiseJSON a1 d1) (MkRowWiseJSON _ d2) =
    MkRowWiseJSON a1 (d1 <> d2)

rowWiseJson :: RowWise -> RowWiseJSON
rowWiseJson (MkRowWise a rd) = MkRowWiseJSON (fmap toJSON a) (fmap toJSON rd)

shapeRowWise :: CohortData Featureset -> RowWise
shapeRowWise x = MkRowWise
  (fmap (nameAttr . NE.head . getFeatureset) cd)
  (fmap MkIDRow (zip ids (fmap (toList . (fmap dataOnly . getFeatureset)) cd)))
 where
  cd  = getCohortDataData x
  ids = getCohortDataIDs x
