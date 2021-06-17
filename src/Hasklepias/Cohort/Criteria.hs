{-|
Module      : Cohort Criteria
Description : Defines the Criteria and related types and functions
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}

{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Hasklepias.Cohort.Criteria(
      Criterion
    , Criteria(..)
    , Status(..)
    , CohortStatus(..)
    , criterion
    , criteria
    , excludeIf
    , includeIf
    , initStatusInfo
    , checkCohortStatus
) where

import safe GHC.Generics                ( Generic )
import safe GHC.Num                     ( Num((+)), Natural )
import safe GHC.Show                    ( Show(show) )
import safe GHC.TypeLits                ( KnownSymbol, symbolVal )
import safe Control.Monad               ( Functor(..) )
import safe Data.Bifunctor              ( Bifunctor(second) )
import safe Data.Bool                   ( Bool(..), otherwise, not, (&&) )
import safe Data.Either                 ( either )
import safe Data.Eq                     ( Eq(..) )
import safe Data.Function               ( ($), (.), const, id )
import safe qualified Data.List.NonEmpty as NE
                                        ( NonEmpty, zip, fromList, toList, map )
import safe Data.List                   ( find, (++) )
import safe Data.Maybe                  ( Maybe(..), maybe )
import safe Data.Ord                    ( Ord(..), Ordering(..) )
import safe Data.Proxy                  ( Proxy )
import safe Data.Tuple                  ( fst, snd )
import safe Data.Text                   ( Text, pack )
import safe FeatureCompose              ( getFeatureData
                                        , Feature
                                        , nameFeature
                                        , FeatureNamed(..) )



data Status = Include | Exclude deriving (Eq, Show)

data CohortStatus=
  Included | ExcludedBy (Natural, Text)
    deriving (Eq, Show, Generic)

instance Ord CohortStatus where
  compare Included Included = EQ
  compare Included (ExcludedBy _) = GT
  compare (ExcludedBy _) Included = LT
  compare (ExcludedBy (i, _)) (ExcludedBy (j, _)) = compare i j

includeIf :: Bool -> Status
includeIf True  = Include
includeIf False = Exclude

excludeIf :: Bool -> Status
excludeIf True  = Exclude
excludeIf False = Include

newtype Criterion = MkCriterion (FeatureNamed Status) deriving (Eq, Show)

criterion :: (KnownSymbol n) => Feature n Status -> Criterion
criterion x = MkCriterion (nameFeature x)

newtype Criteria = MkCriteria {getCriteria :: NE.NonEmpty (Natural, Criterion)} 
    deriving (Eq, Show)

criteria :: NE.NonEmpty Criterion -> Criteria
criteria l = MkCriteria $ NE.zip (NE.fromList [1..]) l

getStatus :: Criterion -> (Text, Status)
getStatus (MkCriterion x) =
  either (const (nm, Exclude)) (nm,) ((getFeatureData . getData') x)
    where nm = getName' x

getStatuses ::
  Criteria -> NE.NonEmpty (Natural, Text, Status)
getStatuses (MkCriteria x) =
  fmap (\c -> (fst c, (fst.getStatus.snd) c, (snd.getStatus.snd) c)) x

findExclude ::
  Criteria -> Maybe (Natural, Text, Status)
findExclude x =  find (\(_, _, z) -> z == Exclude) (getStatuses x)

checkCohortStatus ::
  Criteria -> CohortStatus
checkCohortStatus x =
    maybe Included (\(i, n, _) -> ExcludedBy (i, n)) (findExclude x)

getCriterionName :: Criterion -> Text
getCriterionName (MkCriterion x) = getName' x

initStatusInfo :: Criteria -> [CohortStatus]
initStatusInfo (MkCriteria z) = 
  NE.toList (NE.map (ExcludedBy . Data.Bifunctor.second getCriterionName) z ) ++ [Included]