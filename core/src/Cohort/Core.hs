{-|
Module      : Hasklepias Cohorts
Description : Defines the Cohort type and associated methods
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE Safe #-}

module Cohort.Core
  ( Subject(..)
  , ID
  , Population(..)
  , ObsUnit(..)
  , CohortData(..)
  , Cohort(..)
  , CohortSpec
  , CohortSetSpec
  , CohortSet(..)
  , AttritionInfo(..)
  , AttritionLevel(..)
  , specifyCohort
  , makeObsUnitFeatures
  , evalCohort
  , getCohortIDs
  , getCohortDataIDs
  , getCohortData
  , getCohortDataData
  , getAttritionInfo
  , makeCohortSpecs
  , evalCohortSet
  , getCohortSet
  ) where

import           Cohort.Criteria                ( CohortStatus(..)
                                                , Criteria(getCriteria)
                                                , checkCohortStatus
                                                , initStatusInfo
                                                )
import           Cohort.Index                   ( Index(..)
                                                , makeIndex
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON(..)
                                                )
import           Data.Bool                      ( Bool )
import           Data.Eq                        ( Eq )
import           Data.Foldable                  ( Foldable(length) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>)
                                                , Functor(fmap)
                                                )
import           Data.List                      ( replicate
                                                , zip
                                                , zipWith
                                                )
import qualified Data.List.NonEmpty            as NEL
                                                ( NonEmpty(..)
                                                , toList
                                                )
import           Data.Map.Strict               as Map
                                                ( Map
                                                , fromList
                                                , fromListWith
                                                , toList
                                                , unionsWith
                                                )
import           Data.Maybe                     ( Maybe(..)
                                                , maybe
                                                , catMaybes
                                                )
import           Data.Monoid                    ( mempty )
import           Data.Ord                       ( Ord(..) )
import           Data.Semigroup                 ( Semigroup((<>)) )
import qualified Data.Set                      as Set
                                                ( Set
                                                , fromList
                                                , toList
                                                )
import           Data.Text                      ( Text )
import           Data.Tuple                     ( uncurry )
import           GHC.Generics                   ( Generic )
import           GHC.Int                        ( Int )
import           GHC.Num                        ( Natural
                                                , Num((+))
                                                )
import           GHC.Show                       ( Show(..) )
import           Safe                           ( headMay )
-- | A subject identifier. Currently, simply @Text@.
type ID = Text

-- | A subject is just a pair of @ID@ and data.
newtype Subject d = MkSubject (ID, d)
    deriving (Eq, Show, Generic)

instance Functor Subject where
  fmap f (MkSubject (id, x)) = MkSubject (id, f x)

instance (FromJSON d) => FromJSON (Subject d) where

-- | A population is a list of @'Subject'@s
newtype Population d = MkPopulation  [Subject d]
    deriving (Eq, Show, Generic)

instance Functor Population where
  fmap f (MkPopulation x) = MkPopulation (fmap (fmap f) x)

instance (FromJSON d) => FromJSON (Population d) where

-- | An observational unit is what a subject may be transformed into.
data ObsUnit d = MkObsUnit
  { obsID   :: ID
  , obsData :: d
  }
  deriving (Eq, Show, Generic)

-- | A container for CohortData
newtype CohortData d = MkCohortData { getObsData :: [ObsUnit d] }
    deriving (Eq, Show, Generic)

-- | A cohort is a list of observational units along with @'AttritionInfo'@ 
-- regarding the number of subjects excluded by the @'Criteria'@. 
newtype Cohort d = MkCohort (AttritionInfo, CohortData d)
    deriving (Eq, Show, Generic)

-- | Gets the attrition info from a cohort
getAttritionInfo :: Cohort d -> AttritionInfo
getAttritionInfo (MkCohort (x, _)) = x

-- | Unpacks a @'Population'@ to a list of subjects.
getPopulation :: Population d -> [Subject d]
getPopulation (MkPopulation x) = x

-- | Gets the data out of  a @'Subject'@.
getSubjectData :: Subject d -> d
getSubjectData (MkSubject (_, x)) = x

-- | Tranforms a @'Subject'@ into a @'ObsUnit'@.
makeObsUnitFeatures :: (d1 -> d0) -> Subject d1 -> ObsUnit d0
makeObsUnitFeatures f (MkSubject (id, dat)) = MkObsUnit id (f dat)

-- | A cohort specification consist of two functions: one that transforms a subject's
-- input data into a @'Criteria'@ and another that transforms a subject's input data
-- into the desired return type.
data CohortSpec d1 d0 = MkCohortSpec
  { runCriteria :: d1 -> Criteria
        -- (Feature (Index i a))
  , runFeatures :: d1 -> d0
  }

-- | Creates a @'CohortSpec'@.
specifyCohort :: (d1 -> Criteria) -> (d1 -> d0) -> CohortSpec d1 d0
specifyCohort = MkCohortSpec

-- | Evaluates the @'runCriteria'@ of a @'CohortSpec'@ on a @'Population'@ to 
-- return a list of @Subject Criteria@ (one per subject in the population). 
evalCriteria :: CohortSpec d1 d0 -> Population d1 -> [Subject Criteria]
evalCriteria (MkCohortSpec runCrit _) (MkPopulation pop) =
  fmap (fmap runCrit) pop

-- | Convert a list of @Subject Criteria@ into a list of @Subject CohortStatus@
evalCohortStatus :: [Subject Criteria] -> [Subject CohortStatus]
evalCohortStatus = fmap (fmap checkCohortStatus)

-- | Runs the input function which transforms a subject into an observational unit. 
-- If the subeject is excluded, the result is @Nothing@; otherwise it is @Just@ 
-- an observational unit.
evalSubjectCohort
  :: (d1 -> d0) -> Subject CohortStatus -> Subject d1 -> Maybe (ObsUnit d0)
evalSubjectCohort f (MkSubject (id, status)) subjData = case status of
  Included     -> Just $ makeObsUnitFeatures f subjData
  ExcludedBy _ -> Nothing

-- | A type which collects counts of a 'CohortStatus'
data AttritionLevel = MkAttritionLevel
  { attritionLevel :: CohortStatus
  , attritionCount :: Natural
  }
  deriving (Eq, Show, Generic)

-- | Ordering of @AttritionLevel@ is based on the value of its 'attritionLevel'. 
instance Ord AttritionLevel where
  compare (MkAttritionLevel l1 _) (MkAttritionLevel l2 _) = compare l1 l2

-- | NOTE: the @Semigroup@ instance prefers the 'attritionLevel' from the left,
--   so be sure that you're combining 
instance Semigroup AttritionLevel where
  (<>) (MkAttritionLevel l1 c1) (MkAttritionLevel _ c2) =
    MkAttritionLevel l1 (c1 + c2)

-- | A type which collects the counts of subjects included or excluded.
data AttritionInfo = MkAttritionInfo
  { totalProcessed :: Int
  , attritionInfo  :: Set.Set AttritionLevel
  }
  deriving (Eq, Show, Generic)

setAttrLevlToMap :: Set.Set AttritionLevel -> Map.Map CohortStatus Natural
setAttrLevlToMap x =
  Map.fromList $ (\(MkAttritionLevel l c) -> (l, c)) <$> Set.toList x

mapToSetAttrLevel :: Map.Map CohortStatus Natural -> Set.Set AttritionLevel
mapToSetAttrLevel x = Set.fromList $ uncurry MkAttritionLevel <$> Map.toList x

-- | Two @AttritionInfo@ values can be combined, but this meant for combining
--   attrition info from the same set of @Criteria@.
instance Semigroup AttritionInfo where
  (<>) (MkAttritionInfo t1 i1) (MkAttritionInfo t2 i2) = MkAttritionInfo
    (t1 + t2)
    ( mapToSetAttrLevel
    $ unionsWith (+) [setAttrLevlToMap i1, setAttrLevlToMap i2]
    )

-- Initializes @AttritionInfo@ from a @'Criteria'@.
initAttritionInfo :: Criteria -> Map.Map CohortStatus Natural
initAttritionInfo x = Map.fromList
  $ zip (NEL.toList (initStatusInfo x)) (replicate (length (getCriteria x)) 0)

-- An internal function used to measure attrition for a cohort.
measureAttrition
  :: Maybe Criteria -> [Subject CohortStatus] -> AttritionInfo
measureAttrition c l =
   MkAttritionInfo (length l) $ mapToSetAttrLevel $ unionsWith
    (+)
    [ maybe mempty initAttritionInfo c
    , Map.fromListWith (+) $ fmap (\x -> (getSubjectData x, 1)) l
    , Map.fromList [(Included, 0)]
        -- including Included in the case that none of the evaluated criteria
        -- have status Include
    ]

-- | The internal function to evaluate a @'CohortSpec'@ on a @'Population'@. 
evalUnits
  :: CohortSpec d1 d0 -> Population d1 -> (AttritionInfo, CohortData d0)
evalUnits spec pop =
  ( measureAttrition fcrit statuses
  , MkCohortData $ catMaybes $ zipWith (evalSubjectCohort (runFeatures spec))
                                       statuses
                                       (getPopulation pop)
  )
 where
  crits    = evalCriteria spec pop
  fcrit    = fmap getSubjectData (headMay crits)
  statuses = evalCohortStatus crits

-- | Evaluates a @'CohortSpec'@ on a @'Population'@.
evalCohort :: CohortSpec d1 d0 -> Population d1 -> Cohort d0
evalCohort s p = MkCohort $ evalUnits s p

-- | Get IDs from 'CohortData'.
getCohortDataIDs :: CohortData d -> [ID]
getCohortDataIDs (MkCohortData x) = fmap obsID x

-- | Get IDs from a cohort.
getCohortIDs :: Cohort d -> [ID]
getCohortIDs (MkCohort (_, dat)) = getCohortDataIDs dat

-- | Get data from a cohort.
getCohortDataData :: CohortData d -> [d]
getCohortDataData (MkCohortData x) = fmap obsData x

-- | Get data from a cohort.
getCohortData :: Cohort d -> [d]
getCohortData (MkCohort (_, dat)) = getCohortDataData dat

{-| A container hold multiple cohorts of the same type. The key is the name of 
    the cohort; value is a cohort.
-}
newtype CohortSet d = MkCohortSet (Map Text (Cohort d))
  deriving (Eq, Show, Generic)

-- | Unwraps a 'CohortSet'.
getCohortSet :: CohortSet d -> Map Text (Cohort d)
getCohortSet (MkCohortSet x) = x

{-| Key/value pairs of 'CohortSpec's. The keys are the names of the cohorts.
-}
newtype CohortSetSpec i d = MkCohortSetSpec (Map Text (CohortSpec i d))

-- | Make a set of 'CohortSpec's from list input.
makeCohortSpecs :: [(Text, d1 -> Criteria, d1 -> d0)] -> CohortSetSpec d1 d0
makeCohortSpecs l =
  MkCohortSetSpec $ fromList (fmap (\(n, c, f) -> (n, specifyCohort c f)) l)

-- | Evaluates a @'CohortSetSpec'@ on a @'Population'@.
evalCohortSet :: CohortSetSpec d1 d0 -> Population d1 -> CohortSet d0
evalCohortSet (MkCohortSetSpec s) p = MkCohortSet $ fmap (`evalCohort` p) s