{-|
Module      : Cohort output (and some input)
Description : Methods for outputting a cohort
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
-- {-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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

import           Cohort.Core                    ( Cohort(..)
                                                , CohortData
                                                , CohortSet
                                                , ObsID
                                                , ObsUnit
                                                , getCohortData
                                                , getCohortDataData
                                                , getCohortDataIDs
                                                , getCohortIDs
                                                )
import           Cohort.Criteria                ( AttritionInfo
                                                , CohortStatus
                                                )
import           Data.Aeson                     ( (.=)
                                                , FromJSON
                                                , ToJSON(..)
                                                , Value
                                                , object
                                                )
import           Data.Aeson.Types               ( FromJSON )
import           Data.List.NonEmpty            as NE
                                                ( NonEmpty(..)
                                                , head
                                                , nonEmpty
                                                , toList
                                                )
import           Data.Map.Strict               as Data.Map
                                                ( Map
                                                , unionWith
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
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
import           GHC.Types                      ( Type )
import           Safe                           ( headMay )


instance (ToJSON i) => ToJSON (ObsID i) where
instance (ToJSON d, ToJSON a ) => ToJSON (ObsUnit d a) where
instance (ToJSON d, ToJSON a) => ToJSON (CohortData d a) where
instance (ToJSON d, ToJSON a) => ToJSON (Cohort d a) where

-- | A type used to determine the output shape of a Cohort.
data CohortDataShape d where
  ColumnWise ::(Show a, ToJSON a) => a -> CohortDataShape (ColumnWise i)
  RowWise ::(Show a, ToJSON a) => a -> CohortDataShape (RowWise i)

deriving instance Show d => Show (CohortDataShape d)

-- | Maps CohortDataShape into an Aeson Value. 
toJSONCohortDataShape :: CohortDataShape shape -> Value
toJSONCohortDataShape (ColumnWise x) = toJSON x
toJSONCohortDataShape (RowWise    x) = toJSON x

{- | 
A type containing all the information of a 'Cohort' but where the 'CohortData'
has been reshaped to a 'CohortDataShapeJSON'.
-}
newtype CohortJSON = MkCohortJSON (Maybe AttritionInfo, CohortDataShapeJSON)
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
class ShapeCohort d a where
  colWise :: Cohort d a -> CohortJSON
  rowWise :: Cohort d a -> CohortJSON

instance (ToJSON i ) => ShapeCohort Featureset i where
  colWise (MkCohort (a, d)) =
    MkCohortJSON (a, CW $ colWiseJson (shapeColumnWise d))
  rowWise (MkCohort (a, d)) =
    MkCohortJSON (a, RW $ rowWiseJson (shapeRowWise d))

---- ColumnWise ---- 

data ColumnWise i = MkColumnWise [OutputShape Type] -- attributes
                                 [ObsID i] -- ids
                                 [[OutputShape Type]] -- data
  deriving (Show, Generic)

instance (ToJSON i ) => ToJSON (ColumnWise i) where

-- | A type to hold 'Cohort' information in a column-wise manner.
data ColumnWiseJSON = MkColumnWiseJSON
  { attributes :: [Value]
  , ids        :: [Value]
  , cohortData :: [[Value]]
  }
  deriving (Eq, Show, Generic)

instance ToJSON   ColumnWiseJSON
instance FromJSON ColumnWiseJSON

instance Semigroup ColumnWiseJSON where
  (<>) (MkColumnWiseJSON a1 i1 d1) (MkColumnWiseJSON _ i2 d2) =
    MkColumnWiseJSON a1 (i1 <> i2) (zipWith (<>) d1 d2)

colWiseJson :: (ToJSON i) => ColumnWise i -> ColumnWiseJSON
colWiseJson (MkColumnWise a ids cd) =
  MkColumnWiseJSON (fmap toJSON a) (fmap toJSON ids) (fmap (fmap toJSON) cd)

shapeColumnWise :: CohortData Featureset i -> ColumnWise i
shapeColumnWise x = MkColumnWise (fromMaybe [] attr)
                                 (getCohortDataIDs x)
                                 (fromMaybe [[]] dat)
 where
  feat = fmap (getFeaturesetList . (tpose . MkFeaturesetList))
              (nonEmpty (getCohortDataData x))
  attr = fmap (toList . fmap (nameAttr . NE.head . getFeatureset)) feat
  dat  = fmap (toList . fmap (toList . (fmap dataOnly . getFeatureset))) feat

---- Rowwise ---- 

newtype IDRow i = MkIDRow (ObsID i, [OutputShape Type])
  deriving ( Show, Generic )

instance (ToJSON i) => ToJSON (IDRow i) where
  -- toJSON (MkIDRow x) = object [uncurry (.=) x]

data RowWise i = MkRowWise [OutputShape Type] -- attributes
                                              [IDRow i]  -- data
  deriving (Show, Generic)

instance (ToJSON i) => ToJSON (RowWise i) where

-- | A type to hold 'Cohort' information in a row-wise manner.
data RowWiseJSON = MkRowWiseJSON
  { attributes :: [Value]
  , cohortData :: [Value]
  }
  deriving (Eq, Show, Generic)

instance ToJSON   RowWiseJSON
instance FromJSON RowWiseJSON
instance Semigroup RowWiseJSON where
  (<>) (MkRowWiseJSON a1 d1) (MkRowWiseJSON _ d2) = MkRowWiseJSON a1 (d1 <> d2)

rowWiseJson :: (ToJSON i) => RowWise i -> RowWiseJSON
rowWiseJson (MkRowWise a rd) = MkRowWiseJSON (fmap toJSON a) (fmap toJSON rd)

shapeRowWise :: CohortData Featureset i -> RowWise i
shapeRowWise x = MkRowWise
  (maybe [] (fmap nameAttr . toList . getFeatureset) (headMay cd))
  (fmap MkIDRow (zip ids (fmap (toList . (fmap dataOnly . getFeatureset)) cd)))
 where
  cd  = getCohortDataData x
  ids = getCohortDataIDs x
