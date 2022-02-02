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

module Cohort.Core
  ( Subject(..)
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
import           Control.Applicative            ( (<$>)
                                                , pure
                                                )
import           Control.Monad                  ( (=<<)
                                                , Functor(fmap)
                                                , join
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
                                                , catMaybes
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
import           Data.Traversable               ( sequenceA )
import           Data.Tuple                     ( curry )
import           GHC.Exts                       ( IsList(..) )
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( Natural )
import           GHC.Show                       ( Show(..) )
import           Safe                           ( headMay )


-- | An observational unit identifier. Currently, simply @Text@.
newtype ObsID = MkObsID (Text, Natural)
  deriving (Eq, Show, Generic)

-- | Smart constructor for @'ObsID'@.
makeObsID :: Natural -> Text -> ObsID
makeObsID = (flip . curry) MkObsID

-- | A subject is just a pair of @ID@ and data.
newtype Subject d = MkSubject (Text, d)
    deriving (Eq, Show, Generic)

instance Functor Subject where
  fmap f (MkSubject (id, x)) = MkSubject (id, f x)

instance (FromJSON d) => FromJSON (Subject d) where

-- | A population is a list of @'Subject'@s
newtype Population d = MkPopulation [Subject d]
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
evalIndicesOnSubjects
  :: CohortSpec d1 d0 i a -> Population d1 -> [(IndexSet i a, Subject d1)]
evalIndicesOnSubjects spec (MkPopulation pop) =
  fmap (\x -> (runIndices spec (getSubjectData x), x)) pop

data Step1 d i a = MkStep1 (ObsUnit d) (Index i a)

doStep1 :: (IndexSet i a, Subject d1) -> [Maybe (Step1 d1 i a)]
doStep1 (MkIndexSet indices, MkSubject (id, d)) = case indices of
  Nothing  -> pure Nothing
  Just set -> zipWith
    (\x y -> Just $ MkStep1 (MkObsUnit (makeObsID y id) d) x)
    (Set.toList set)
    [1 ..]

  -- Nothing  -> pure $ MkStep1 (MkObsUnit (makeObsID 1 id) d) Nothing
  -- Just set -> zipWith
  --   (\x y -> MkStep1 (MkObsUnit (makeObsID y id) d) (Just x))
  --   (Set.toList set)
  --   [1 ..]

-- | Evaluates the @'runIndices'@ of a @'CohortSpec'@ on a @'Population'@ to 
-- return a list of @(Index i a, ObsUnit d)@ (one per observational unit). 
evalIndices :: CohortSpec d1 d0 i a -> Population d1 -> [Maybe (Step1 d1 i a)]
evalIndices spec x = doStep1 =<< evalIndicesOnSubjects spec x

newtype Step2 d i a = MkStep2 (Criteria, Step1 d i a)

-- | Evaluates the @'runCriteria'@ of a @'CohortSpec'@ on a @'Population'@ to 
-- return a list of @(Criteria, Index i a, ObsUnit d1)@. 
evalCriteria
  :: CohortSpec d1 d0 i a -> [Maybe (Step1 d1 i a)] -> [Maybe (Step2 d1 i a)]
evalCriteria spec = fmap
  (fmap
    (\(MkStep1 ou i) -> MkStep2 (runCriteria spec i (obsData ou), MkStep1 ou i))
  )

data Step3 d i a =
    I1 CohortStatus
  | I2 CohortStatus (ObsUnit d) (Index i a)

doStep3 :: Maybe (Step2 d i a) -> Step3 d i a
doStep3 (Just (MkStep2 (c, MkStep1 ou i))) = case (status, i) of
  (Included, index) -> I2 status ou index
  _                 -> I1 status
  where status = checkCohortStatus (Just i) c
doStep3 Nothing = I1 SubjectHasNoIndex

step3toStatus :: Step3 d i a -> CohortStatus
step3toStatus (I1 x    ) = x
step3toStatus (I2 x _ _) = x

-- | Convert a list of @(Criteria, Index i a, ObsUnit d)@ into a list of 
--   @(CohortStatus, Index i a, ObsUnit d)@
evalCohortStatus :: [Maybe (Step2 d i a)] -> [Step3 d i a]
evalCohortStatus = fmap doStep3

-- | Internal function for mapping an @ObsUnit d1@ along with its corresponding 
--   @CohortStatus@ and @Index@ into @Maybe (ObsUnit d0)@. 
evalFeatures :: (Index i a -> d1 -> d0) -> Step3 d1 i a -> Maybe (ObsUnit d0)
evalFeatures f x = case x of
  I2 status obsUnit index -> Just $ fmap (f index) obsUnit
  _                       -> Nothing

-- | The internal function to evaluate a @'CohortSpec'@ on a @'Population'@. 
evalCohort :: CohortSpec d1 d0 i a -> Population d1 -> Cohort d0
evalCohort spec pop = MkCohort
  ( measureAttrition firstCrit statuses
  , MkCohortData $ mapMaybe (evalFeatures (runFeatures spec)) unitsWithStatus
  )
 where
  indices         = evalIndices spec pop
  unitsWithCrits  = evalCriteria spec indices
  unitsWithStatus = evalCohortStatus unitsWithCrits
  firstCrit = (\(MkStep2 (c, _)) -> c) <$> headMay (catMaybes unitsWithCrits)
  statuses        = step3toStatus <$> unitsWithStatus

{-| A container hold multiple cohorts of the same type. The key is the name of 
    the cohort; value is a cohort.
-}
newtype CohortSet d = MkCohortSet (Map Text (Cohort d))
  deriving (Eq, Show, Generic)

-- | Unwraps a 'CohortSet'.
getCohortSet :: CohortSet d -> Map Text (Cohort d)
getCohortSet (MkCohortSet x) = x

-- | Key/value pairs of 'CohortSpec's. The keys are the names of the cohorts.
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
