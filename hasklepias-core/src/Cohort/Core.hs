{-|
Module      : Hasklepias Cohorts
Description : Defines the Cohort type and associated methods
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Cohort.Core
  ( Subject
  , ObsID
  , ObsUnit
  , Population
  , CohortData
  , Cohort(..)
  , CohortSpec
  , CohortSetSpec
  , CohortSet
  , CohortEvalOptions
  , makeObsID
  , specifyCohort
  , getCohortIDs
  , getCohortDataIDs
  , getCohortData
  , getCohortDataData
  , getAttritionInfo
  , makeCohortSpecs
  , makeCohortEvaluator
  , makeCohortSetEvaluator
  , defaultCohortEvalOptions
  ) where

import           Cohort.Attrition
import           Cohort.Criteria                ( CohortStatus(..)
                                                , Criteria
                                                , checkCohortStatus
                                                , initStatusInfo
                                                )
import           Cohort.IndexSet
import           Control.Applicative            ( liftA2 )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON(..)
                                                )
import qualified Data.List.NonEmpty            as NE
import           Data.Map.Strict               as Map
                                                ( Map
                                                , elems
                                                , keys
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.Semigroup                 ( sconcat )
import qualified Data.Set                      as Set
                                                ( Set )
import           Data.Text                      ( Text )
import           GHC.Exts                       ( IsList(..) )
import           GHC.Generics                   ( Generic )
import           Safe                           ( headMay )
import           Witch                          ( From(..)
                                                , into
                                                )

{-|
An observational unit identifier. 
The textual representation of a Subject ID,
plus the 'Index' of the unit.
-}
newtype ObsID i = MkObsID (Text, i)
  deriving (Eq, Show, Ord, Generic)

instance From (Text, i) (ObsID i)  where
instance From (ObsID i) (Text, i) where
instance From (ObsID i) i where
  from (MkObsID (x, y)) = y

-- | Smart constructor for @'ObsID'@.
makeObsID :: i -> Text -> ObsID i
makeObsID = (flip . curry) MkObsID

{-|
A @'Subject'@ may be mapped to zero of more observational units ('ObsUnit').
-}
data ObsUnit d i = MkObsUnit
  { obsID   :: ObsID i
  , obsData :: d
  }
  deriving (Eq, Show, Generic)

instance From (ObsID a, d) (ObsUnit d a)  where
  from (x, y) = MkObsUnit x y

{-| 
A subject is just a pair of a 'Text' ID and data.
-}
newtype Subject d = MkSubject (Text, d)
    deriving (Eq, Show, Generic)

-- (Internal) Gets the ID out of  a @'Subject'@.
getSubjectID :: Subject d -> Text
getSubjectID (MkSubject (x, _)) = x

-- (Internal) Gets the data out of  a @'Subject'@.
getSubjectData :: Subject d -> d
getSubjectData (MkSubject (_, x)) = x

instance Functor Subject where
  fmap f (MkSubject (id, x)) = MkSubject (id, f x)

instance (FromJSON d) => FromJSON (Subject d) where

instance From (Text, d) (Subject d) where

{-|
A population is a container of @'Subject'@s.
Currently, the container is a list.
-}
newtype Population d = MkPopulation [Subject d]
    deriving (Eq, Show, Generic)

instance Functor Population where
  fmap f (MkPopulation x) = MkPopulation (fmap (fmap f) x)

instance (FromJSON d) => FromJSON (Population d) where

instance From [Subject d] (Population d) where
instance From (Population d) [Subject d] where

{-| 
A container for CohortData
-}
newtype CohortData d i = MkCohortData [ObsUnit d i]
    deriving (Eq, Show, Generic)

instance From [ObsUnit d i] (CohortData d i) where

{-| 
A cohort is a list of observational units along with @'AttritionInfo'@ 
regarding the number of subjects excluded by the @'Criteria'@..
-}
newtype Cohort d i = MkCohort (Maybe AttritionInfo, CohortData d i)
    deriving (Eq, Show, Generic)

instance From (Cohort d i) (Maybe AttritionInfo, CohortData d i) where

-- | Gets the attrition info from a cohort
getAttritionInfo :: Cohort d a -> Maybe AttritionInfo
getAttritionInfo (MkCohort (x, _)) = x

-- | Get IDs from 'CohortData'.
getCohortDataIDs :: CohortData d a -> [ObsID a]
getCohortDataIDs (MkCohortData x) = fmap obsID x

-- | Get IDs from a cohort.
getCohortIDs :: Cohort d a -> [ObsID a]
getCohortIDs (MkCohort (_, dat)) = getCohortDataIDs dat

-- | Get data from a cohort.
getCohortDataData :: CohortData d a -> [d]
getCohortDataData (MkCohortData x) = fmap obsData x

-- | Get data from a cohort.
getCohortData :: Cohort d a -> [d]
getCohortData (MkCohort (_, dat)) = getCohortDataData dat

{-| 
A cohort specification consist of three functions: 

TODO

-}
data CohortSpec d1 d0 i = MkCohortSpec
  { runIndices  :: d1 -> IndexSet i
  , runCriteria :: i -> d1 -> Criteria
  , runFeatures :: i -> d1 -> d0
  }

{-|
TODO
-}
specifyCohort
  :: (d1 -> IndexSet i)
  -> (i -> d1 -> Criteria)
  -> (i -> d1 -> d0)
  -> CohortSpec d1 d0 i
specifyCohort = MkCohortSpec

{-|
TODO
-}
data EvaluateFeatures =
  -- | TODO
    IncludeFeatures
  -- | TODO
  | SkipFeatures

{-|
TODO
-}
data UnitsToEvaluateFeatures =
    OnAll -- ^ TODO 
  | OnlyOnIncluded -- ^ TODO

{-|
TODO
-}
data CohortEvalOptions = MkCohortEvalOptions
  {
    -- | TODO
    unitsToEvaluateFeatures :: UnitsToEvaluateFeatures
    -- | TODO
  , includeFeatures         :: EvaluateFeatures
  }

{-|
TODO
-}
defaultCohortEvalOptions :: CohortEvalOptions
defaultCohortEvalOptions = MkCohortEvalOptions OnlyOnIncluded IncludeFeatures

{-
*INTERNAL*


-}
data EvaluatedSubject d i =
    SNoIndex Text CohortStatus -- ^ TODO 
  | SUnits [ (ObsID i, CohortStatus, Maybe d) ] -- ^ TODO

instance From (ObsID i, CohortStatus, Maybe d ) (Maybe (ObsUnit d i)) where
  from (i, _, Just d ) = Just $ MkObsUnit i d
  from (_, _, Nothing) = Nothing

instance From (EvaluatedSubject d i) [ObsUnit d i] where
  from (SNoIndex _ _) = []
  from (SUnits x    ) = mapMaybe (into @(Maybe (ObsUnit d i))) x

{-|
TODO
-}
makeSubjectEvaluator
  :: forall m d1 d0 i
   . (Monad m)
  => CohortEvalOptions
  -> CohortSpec d1 d0 i
  -> (Subject d1 -> m (AttritionInfo, EvaluatedSubject d0 i))
makeSubjectEvaluator opts spec subj = do
  let sid = getSubjectID subj
  let sdt = getSubjectData subj

  -- Evaluate indices; 
  -- convert any indices in set to list
  let inx = into @(Maybe [i]) $ runIndices spec sdt

  -- In the case that the subject has no indices,
  -- return one observation.
  -- In the case the subject has 1 or more indices,
  -- return a status for each index
  case inx of
    Nothing ->
      pure (measureAttrition Nothing [], SNoIndex sid SubjectHasNoIndex)
    Just ins -> do

      -- For each index, evaluate criteria
      let crits         = fmap (flip (runCriteria spec) sdt) ins

      -- For each index/criteria evaluate cohort status
      -- checkCohortStatus has sig:: Maybe (Index a) -> Criteria -> CohortStatus
      -- Thus use of Just here to lift an Index back into Maybe
      let stats         = zipWith (checkCohortStatus . Just) ins crits

      -- Pair up each status with its index for ease of processing later.
      let statusIndices = liftA2 (\x y -> (makeObsID y sid, x)) stats ins

      -- Measure the contribution to attrition for this subject
      let attrition     = measureAttrition (headMay crits) stats

      -- Create a function which applies a function g
      -- to the statusIndices and returns the desired result type
      let doFeatures g = pure (attrition, SUnits $ fmap g statusIndices)

      -- Here we need to handle user options.
      case includeFeatures opts of
        -- If the user want to evaluate the features, 
        -- there are then the user options for which subjects
        -- they would like to keep in the output.
        IncludeFeatures -> do

          -- A function which takes an index and runs the 
          -- features given the cohort spec and subject's data.
          let featureRunner i = runFeatures spec i sdt

          case unitsToEvaluateFeatures opts of
          -- here the user chooses to evaluate features
          -- on all observational units that have an index
            OnAll -> do
              doFeatures (\x -> tackOn (Just $ featureRunner (from (fst x))) x)
            -- here the user chooses to evaluate features
            -- only on those units whose dispositon is Included
            OnlyOnIncluded -> do
              doFeatures
                (\x -> case snd x of
                  Included -> tackOn (Just $ featureRunner (from (fst x))) x
                  _        -> tackOn Nothing x
                )
      -- If the user chooses to skip evaluating features
        -- we return Nothing in the data slot.
        SkipFeatures -> doFeatures (tackOn Nothing)
  where tackOn z (x, y) = (x, y, z)

{-| 
TODO
-}
makePopulationEvaluator
  :: forall m d1 d0 i
   . Monad m
  => CohortEvalOptions
  -> CohortSpec d1 d0 i
  -> (  Population d1
     -> m (Maybe AttritionInfo, [EvaluatedSubject d0 i])
     )
makePopulationEvaluator opts spec pop = do
  let evalSubject = makeSubjectEvaluator opts spec
  let subjects    = into @[Subject d1] pop

  pure $ (\x -> (fmap sconcat $ NE.nonEmpty $ fst x, snd x))
    (unzip (evalSubject =<< subjects))

{-| 
TODO
-}
makeCohortEvaluator
  :: forall m d1 d0 i
   . Monad m
  => CohortEvalOptions
  -> CohortSpec d1 d0 i
  -> (Population d1 -> m (Cohort d0 i))
makeCohortEvaluator opts spec pop =

  let evalPopulation = makePopulationEvaluator opts spec
  in
    (do
      ePop <- evalPopulation pop
      pure $ MkCohort
        (fst ePop, MkCohortData $ (into @[ObsUnit d0 i]) =<< snd ePop)
    )

{-| 
A container hold multiple cohorts of the same type.
The key is the name of the cohort; value is a cohort.
-}
newtype CohortSet d i = MkCohortSet (Map Text (Cohort d i))
  deriving (Eq, Show, Generic)

instance From (CohortSet d i) (Map Text (Cohort d i)) where

{-| 
Key/value pairs of 'CohortSpec's. 
The keys are the names of the cohorts.
-}
newtype CohortSetSpec d1 d0 i = MkCohortSetSpec (Map Text (CohortSpec d1 d0 i))

{-| 
Make a set of 'CohortSpec's from list input.
-}
makeCohortSpecs
  :: [(Text, d1 -> IndexSet i, i -> d1 -> Criteria, i -> d1 -> d0)]
  -> CohortSetSpec d1 d0 i
makeCohortSpecs l = MkCohortSetSpec
  $ fromList (fmap (\(n, i, c, f) -> (n, specifyCohort i c f)) l)

{-|
Evaluates a @'CohortSetSpec'@ on a @'Population'@.
-}
makeCohortSetEvaluator
  :: forall m d1 d0 i
   . Monad m
  => CohortEvalOptions
  -> CohortSetSpec d1 d0 i
  -> Population d1
  -> m (CohortSet d0 i)
makeCohortSetEvaluator opts (MkCohortSetSpec specs) pop = do
  let doCohort s = makeCohortEvaluator opts s pop
  let cohorts = fmap (\(k, v) -> (k, ) =<< doCohort v) (toList specs)
  pure $ MkCohortSet $ fromList cohorts
