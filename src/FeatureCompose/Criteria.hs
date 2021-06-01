{-|
Module      : Feature Building Criteria
Description : Defines the Feature type and its component types, constructors, 
              and class instances
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}

module FeatureCompose.Criteria(
      Criterion(..)
    , Criteria(..)
    , Status(..)
    , getBools
    , include
    , exclude
    , collectBools
    , evalBools
    , runCriteria
) where

import safe GHC.Int                    ( Int )
import safe GHC.Num                    ( Num((+)) )
import safe GHC.Show                   ( Show(show) )
import safe Control.Monad              ( Functor(..) )
import safe Data.Bool                  ( Bool(..), otherwise, not, (&&) )
import safe Data.Either                ( Either(..), partitionEithers )
import safe Data.Eq                    ( Eq(..) )
import safe Data.Function              ( ($), (.) )
import safe Data.List                  ( all, transpose, null, elemIndex )
import safe Data.Maybe                 ( Maybe(..), maybe )
import safe Data.Tuple                 ( fst, snd )
import safe Data.Text                  ( Text )
import safe FeatureCompose

data Criterion b =
      Inclusion (Feature b Bool)
    | Exclusion (Feature b Bool)
    deriving (Show, Eq)

newtype Criteria b = MkCriteria [Criterion b] deriving (Show)

data Status = Included | ExcludedBy Int deriving (Show, Eq)

statusMay :: Maybe Int -> Status
statusMay (Just i) = ExcludedBy i
statusMay Nothing  = Included

getBools :: (Show b) => Criterion b -> FeatureData Bool
getBools (Inclusion x) = getData x
getBools (Exclusion x) = fmap not (getData x)

include :: (Show b) => Feature b Bool -> Criterion b
include = Inclusion

exclude :: (Show b) => Feature b Bool -> Criterion b
exclude = Exclusion

collectBools :: (Show b) => Criteria b -> [FeatureData Bool]
collectBools (MkCriteria x) = fmap getBools x

runBools :: [[Bool]] -> [Status]
runBools l = fmap (statusMay . fmap (+1) . elemIndex False) (transpose l)

-- | TODO: what happens if the feature data lists are not the same length or empty(?)
--         this should get handled in a safer way.
evalBools :: [FeatureData Bool] -> FeatureData Status
evalBools fs
    | null (fst bools) = featureDataR $ runBools (snd bools)
    | otherwise        = featureDataL Excluded
    where bools = partitionEithers $ fmap getFeatureData fs

runCriteria :: (Show b) => Criteria b -> FeatureData Status
runCriteria  = evalBools . collectBools
