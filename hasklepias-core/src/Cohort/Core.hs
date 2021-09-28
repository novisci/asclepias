{-|
Module      : Hasklepias Cohorts
Description : Defines the Cohort type and associated methods
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE Safe #-}

module Cohort.Core
  ( Subject(..)
  , SubjectID
  , ObsID
  , Population(..)
  , ObsUnit(..)
  , CohortData(..)
  , Cohort(..)
  , CohortSpec
  , CohortSetSpec
  , CohortSet(..)
  , makeObsID
  , specifyCohort
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

import           Cohort.Attrition
import           Cohort.Criteria                ( CohortStatus(..)
                                                , Criteria(getCriteria)
                                                , checkCohortStatus
                                                , initStatusInfo
                                                )
import           Cohort.Index
import           Control.Applicative            ( (<$>) )
import           Control.Monad                  ( (=<<)
                                                , Functor(fmap)
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON(..)
                                                )
import           Data.Bool                      ( Bool )
import           Data.Eq                        ( Eq )
import           Data.Foldable                  ( Foldable(length) )
import           Data.Function                  ( ($)
                                                , (.)
                                                , flip
                                                )
import           Data.List                      ( zipWith )
import           Data.Map.Strict               as Map
                                                ( Map )
import           Data.Maybe                     ( Maybe(..)
                                                , mapMaybe
                                                )
import           Data.Monoid                    ( mempty )
import           Data.Ord                       ( Ord(..) )
import           Data.Semigroup                 ( Semigroup((<>)) )
import qualified Data.Set                      as Set
                                                ( Set
                                                , toList
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Tuple                     ( curry )
import           GHC.Exts                       ( IsList(..) )
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( Natural )
import           GHC.Show                       ( Show(..) )
import           Safe                           ( headMay )


-- | A subject identifier. Currently, simply @Text@.
type SubjectID = Text

-- | An observational unit identifier. Currently, simply @Text@.
-- type ObsID = Text
newtype ObsID = MkObsID (SubjectID, Natural)
  deriving (Eq, Show, Generic)

-- | A subject is just a pair of @ID@ and data.
newtype Subject d = MkSubject (SubjectID, d)
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

-- | A @'Subject'@ may be mapped to zero of more observational units (one per
--   @'Index'@ in the @'IndexSet'@).
data ObsUnit d = MkObsUnit
  { obsID   :: ObsID
  , obsData :: d
  }
  deriving (Eq, Show, Generic)

instance Functor ObsUnit where
  fmap f (MkObsUnit id x) = MkObsUnit id (f x)

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

-- | Get IDs from 'CohortData'.
getCohortDataIDs :: CohortData d -> [ObsID]
getCohortDataIDs (MkCohortData x) = fmap obsID x

-- | Get IDs from a cohort.
getCohortIDs :: Cohort d -> [ObsID]
getCohortIDs (MkCohort (_, dat)) = getCohortDataIDs dat

-- | Get data from a cohort.
getCohortDataData :: CohortData d -> [d]
getCohortDataData (MkCohortData x) = fmap obsData x

-- | Get data from a cohort.
getCohortData :: Cohort d -> [d]
getCohortData (MkCohort (_, dat)) = getCohortDataData dat

-- | Unpacks a @'Population'@ to a list of subjects.
getPopulation :: Population d -> [Subject d]
getPopulation (MkPopulation x) = x

-- | Gets the data out of  a @'Subject'@.
getSubjectData :: Subject d -> d
getSubjectData (MkSubject (_, x)) = x

-- | A cohort specification consist of two functions: one that transforms a subject's
-- input data into a @'Criteria'@ and another that transforms a subject's input data
-- into the desired return type.
data CohortSpec d1 d0 i a = MkCohortSpec
  { runIndices  :: d1 -> IndexSet i a
  , runCriteria :: Index i a -> d1 -> Criteria
  , runFeatures :: Index i a -> d1 -> d0
  }

-- | Creates a @'CohortSpec'@.
specifyCohort
  :: (d1 -> IndexSet i a)
  -> (Index i a -> d1 -> Criteria)
  -> (Index i a -> d1 -> d0)
  -> CohortSpec d1 d0 i a
specifyCohort = MkCohortSpec

-- internal functions to evalIndices
_evalIndices
  :: CohortSpec d1 d0 i a -> Population d1 -> [(IndexSet i a, Subject d1)]
_evalIndices spec (MkPopulation pop) =
  fmap (\x -> (runIndices spec (getSubjectData x), x)) pop

makeObsID :: Natural -> SubjectID -> ObsID
makeObsID =  (flip . curry) MkObsID

g :: (IndexSet i a, Subject d1) -> [(Index i a, ObsUnit d1)]
g (MkIndexSet is, MkSubject (id, d)) =
  zipWith (\x y -> (x, MkObsUnit (makeObsID y id) d)) (Set.toList is) [1..]

-- | Evaluates the @'runIndices'@ of a @'CohortSpec'@ on a @'Population'@ to 
-- return a list of @(Index i a, ObsUnit d)@ (one per observational unit). 
evalIndices
  :: CohortSpec d1 d0 i a -> Population d1 -> [(Index i a, ObsUnit d1)]
evalIndices spec x = g =<< _evalIndices spec x
-- evalIndices spec = flattenIndices . fmap g . _evalIndices spec

-- | Evaluates the @'runCriteria'@ of a @'CohortSpec'@ on a @'Population'@ to 
-- return a list of @(Criteria, Index i a, ObsUnit d1)@. 
evalCriteria
  :: CohortSpec d1 d0 i a
  -> [(Index i a, ObsUnit d1)]
  -> [(Criteria, Index i a, ObsUnit d1)]
evalCriteria spec = fmap (\(i, d) -> (runCriteria spec i (obsData d), i, d))

-- | Convert a list of @(Criteria, Index i a, ObsUnit d)@ into a list of 
--   @(CohortStatus, Index i a, ObsUnit d)@
evalCohortStatus
  :: [(Criteria, Index i a, ObsUnit d)]
  -> [(CohortStatus, Index i a, ObsUnit d)]
evalCohortStatus = fmap (\(x, y, z) -> (checkCohortStatus x, y, z))

-- | Internal function for mapping an @ObsUnit d1@ along with its corresponding 
--   @CohortStatus@ and @Index@ into @Maybe (ObsUnit d0)@. 
evalFeatures
  :: (Index i a -> d1 -> d0)
  -> (CohortStatus, Index i a, ObsUnit d1)
  -> Maybe (ObsUnit d0)
evalFeatures f (status, index, obsUnit) = case status of
  Included     -> Just $ fmap (f index) obsUnit
  ExcludedBy _ -> Nothing

-- | The internal function to evaluate a @'CohortSpec'@ on a @'Population'@. 
evalCohort :: CohortSpec d1 d0 i a -> Population d1 -> Cohort d0
evalCohort spec pop = MkCohort
  ( measureAttrition firstCrit statuses
  , MkCohortData $ mapMaybe (evalFeatures (runFeatures spec)) unitsWithStatus
  )
 where
  indices          = evalIndices spec pop
  unitsWithCrits   = evalCriteria spec indices
  unitsWithStatus  = evalCohortStatus unitsWithCrits
  firstCrit        = (\(x, _, _) -> x) <$> headMay unitsWithCrits
  statuses         = (\(x, _, _) -> x) <$> unitsWithStatus

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
newtype CohortSetSpec d1 d0 i a = MkCohortSetSpec (Map Text (CohortSpec d1 d0 i a))

-- | Make a set of 'CohortSpec's from list input.
makeCohortSpecs
  :: [ ( Text
       , d1 -> IndexSet i a
       , Index i a -> d1 -> Criteria
       , Index i a -> d1 -> d0
       )
     ]
  -> CohortSetSpec d1 d0 i a
makeCohortSpecs l = MkCohortSetSpec
  $ fromList (fmap (\(n, i, c, f) -> (n, specifyCohort i c f)) l)

-- | Evaluates a @'CohortSetSpec'@ on a @'Population'@.
evalCohortSet :: CohortSetSpec d1 d0 i a -> Population d1 -> CohortSet d0
evalCohortSet (MkCohortSetSpec s) p = MkCohortSet $ fmap (`evalCohort` p) s
