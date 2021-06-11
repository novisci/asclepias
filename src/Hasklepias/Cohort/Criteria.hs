{-|
Module      : Cohort Criteria
Description : Defines the Criteria and related types and functions
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Hasklepias.Cohort.Criteria(
      Criterion
    , Criteria
    , CriterionDefinition
    , CriterionSpec
    , Status(..)
    , CohortStatus(..)
    , criterion
    , criteria
    , excludeIf
    , includeIf
    , defineCriterion
    , specifyCriterion
    , evalCriterionSpec
    , checkCohortStatus
) where

import safe GHC.Generics               ( Generic )
import safe GHC.Int                    ( Int )
import safe GHC.Num                    ( Num((+)), Natural )
import safe GHC.Show                   ( Show(show) )
import safe Control.Monad              ( Functor(..) )
import safe Data.Bool                  ( Bool(..), otherwise, not, (&&) )
import safe Data.Either                ( Either(..), partitionEithers, fromRight, either )
import safe Data.Eq                    ( Eq(..) )
import safe Data.Function              ( ($), (.), const, id )
import safe qualified Data.List.NonEmpty as NE
import safe Data.List                  ( all, transpose, null, find, zip)
import safe Data.Maybe                 ( Maybe(..), maybe )
import safe Data.Ord                   ( Ord(..), Ordering(..) )
import safe Data.Tuple                 ( fst, snd )
import safe Data.Text                  ( Text )
import safe FeatureCompose             ( FeatureDefinition,
                                          FeatureData(getFeatureData),
                                          Feature(getName, getData),
                                          FeatureSpec,
                                          specifyFeature,
                                          EvalSpec(..) )


data Status = Include | Exclude deriving (Eq, Show)

data CohortStatus = Included | ExcludedBy (Natural, Text) 
    deriving (Eq, Show, Generic)

instance Ord CohortStatus where
  compare Included Included = EQ 
  compare Included (ExcludedBy _) = GT 
  compare (ExcludedBy _) Included = LT 
  compare (ExcludedBy i) (ExcludedBy j) = compare i j

includeIf :: Bool -> Status
includeIf True  = Include
includeIf False = Exclude

excludeIf :: Bool -> Status
excludeIf True  = Exclude
excludeIf False = Include

newtype CriterionDefinition di = MkCriterionDefinition (FeatureDefinition di Status)

newtype CriterionSpec b di = MkCriterionSpec (FeatureSpec b di Status)

defineCriterion :: FeatureDefinition di Status -> CriterionDefinition di
defineCriterion = MkCriterionDefinition

specifyCriterion :: Show b => Text -> b -> FeatureDefinition di Status -> CriterionSpec b di
specifyCriterion n a def = MkCriterionSpec (specifyFeature n a def)

evalCriterionSpec :: (Show b, EvalSpec di Status) =>
    CriterionSpec b di -> di -> Criterion b
evalCriterionSpec (MkCriterionSpec spec) x = MkCriterion $ evalSpec spec x

newtype Criterion b = MkCriterion (Feature b Status)

criterion :: Feature b Status -> Criterion b
criterion = MkCriterion

newtype Criteria b = MkCriteria (NE.NonEmpty (Natural, Criterion b))

criteria :: NE.NonEmpty (Criterion b) -> Criteria b
criteria l = MkCriteria $ NE.zip (NE.fromList [1..]) l

getStatus :: (Show b) => Criterion b -> (Text, Status)
getStatus (MkCriterion x) =
  either (const (nm, Exclude)) (nm,) ((getFeatureData . getData) x)
    where nm = getName x

getStatuses :: (Show b) => Criteria b -> NE.NonEmpty (Natural, Text, Status)
getStatuses (MkCriteria x) = fmap (\c -> (fst c, (fst.getStatus.snd) c, (snd.getStatus.snd) c)) x

findExclude :: Show b => Criteria b -> Maybe (Natural, Text, Status)
findExclude x =  find (\(_, _, z) -> z == Exclude) (getStatuses x)

checkCohortStatus :: (Show b) => Criteria b -> CohortStatus
checkCohortStatus x =
    maybe Included (\(i, n, _) -> ExcludedBy (i, n)) (findExclude x)

