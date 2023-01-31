{-|
Module      : Cohort output (and some input)
Description : Methods for outputting a cohort
Copyright   : (c) Target RWE 2023
License     : BSD3
Maintainer  : bbrown@targetrwe.com 
              ljackman@targetrwe.com 
              dpritchard@targetrwe.com
-}

{-# LANGUAGE DeriveGeneric #-}

module Cohort.Output where

import           Cohort.Cohort
import           Data.Aeson          (ToJSON (..), Value)
import           Data.List.NonEmpty  as NE (head, nonEmpty, toList)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import           Features.Featureset (FeaturesetList (MkFeaturesetList),
                                      getFeatureset, getFeaturesetList, tpose)
import           Features.Output     (ShapeOutput (dataOnly, nameAttr))
import           GHC.Generics        (Generic)

{- |
A type containing all the information of a 'Cohort' but where the 'CohortData'
has been reshaped to a 'CohortDataShapeJSON'.
-}
data CohortJSON
  = MkCohortJSON
      { attritionJSON :: AttritionInfo
      , cohortJSON    :: ColumnWiseJSON
      }
  deriving (Eq, Generic, Show)

instance ToJSON CohortJSON

{- |
Similar to 'CohortMap', but where the 'Cohort's have been mapped to a 'CohortJSON'.
-}
type CohortMapJSON = Map Text CohortJSON

---- ColumnWise output shape ----

data ColumnWiseJSON
  = MkColumnWiseJSON
      { attributes :: [Value]
      , ids        :: [Value]
      , cohortData :: [[Value]]
      }
  deriving (Eq, Generic, Show)

instance ToJSON   ColumnWiseJSON

shapeColumnWise :: (ToJSON a) => Cohort a -> ColumnWiseJSON
shapeColumnWise x = MkColumnWiseJSON (toJSON <$> fromMaybe [] attr)
                                 (map (toJSON . obsId) cdat)
                                 (map toJSON <$> fromMaybe [[]] dat)
 where
  cdat = Cohort.Cohort.cohortData x
  feat = fmap (getFeaturesetList . (tpose . MkFeaturesetList))
              (nonEmpty (map obsData cdat))
  attr = fmap (toList . fmap (nameAttr . NE.head . getFeatureset)) feat
  dat  = fmap (toList . fmap (toList . (fmap dataOnly . getFeatureset))) feat

toCohortJSON :: (ToJSON a) => Map Text (Cohort a) -> CohortMapJSON
toCohortJSON = M.map op
  where op c = MkCohortJSON (attritionInfo c) (shapeColumnWise c)
