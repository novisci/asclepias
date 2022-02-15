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
{-# LANGUAGE FlexibleContexts #-}

module Cohort.Core
  ( SubjID
  , Subject
  , ObsID
  , ObsUnit
  , Population
  , CohortData
  , Cohort(..)
  , CohortSpec
  , CohortMapSpec
  , CohortSet
  , CohortEvalOptions(..)
  , EvaluateFeatures(..)
  , SubjectSample(..)
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
import           Control.Monad                  ( replicateM )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON(..)
                                                )
import           Data.Bifunctor
import qualified Data.List.NonEmpty            as NE
import           Data.Map.Strict               as Map
                                                ( Map
                                                , elems
                                                , keys
                                                )
import           Data.Semigroup                 ( sconcat )
import qualified Data.Set                      as Set
                                                ( Set )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           GHC.Exts                       ( IsList(..) )
import           GHC.Generics                   ( Generic )
import           Safe                           ( headMay )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , arbitraryASCIIChar
                                                )
import           Witch                          ( From(..)
                                                , into
                                                , via
                                                )
import qualified Witherable                    as W


{-| 
-}
newtype SubjID = MkSubjID Text
  deriving (Eq, Show, Generic)

instance FromJSON SubjID
instance From Text SubjID
instance From SubjID Text
-- | Arbitrary @SubjID@ are simply random strings of 10 characters.
instance Arbitrary SubjID where
  arbitrary = into . pack <$> replicateM 10 arbitraryASCIIChar

-- | Smart constructor for @'SubjID'@.
makeSubjID :: (From t Text) => t -> SubjID
makeSubjID = MkSubjID . into

{-| 
A subject is just a pair of a 'Text' ID and data.
-}
newtype Subject d = MkSubject (SubjID, d)
    deriving (Eq, Show, Generic)

-- (Internal) Gets the ID out of  a @'Subject'@.
getSubjectID :: Subject d -> SubjID
getSubjectID (MkSubject (x, _)) = x

-- (Internal) Gets the data out of  a @'Subject'@.
getSubjectData :: Subject d -> d
getSubjectData (MkSubject (_, x)) = x

instance Functor Subject where
  fmap f (MkSubject (id, x)) = MkSubject (id, f x)

instance (FromJSON d) => FromJSON (Subject d)
instance From (Text, d) (Subject d)
instance (Arbitrary d) => Arbitrary (Subject d) where
  arbitrary = do
    subjid <- arbitrary
    dat    <- arbitrary
    pure $ MkSubject (subjid, dat)

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
instance (Arbitrary d) => Arbitrary (Population d) where
  arbitrary = MkPopulation <$> arbitrary

{-|
An observational unit identifier. 
The textual representation of a Subject ID,
plus the index of the unit,
where 'index' is in the temporal sense of epidemiological studies
and not array indexing as in Ix.
-}
newtype ObsID i = MkObsID (Text, i)
  deriving (Eq, Show, Ord, Generic)

instance From (Text, i) (ObsID i)  where
instance From (ObsID i) (Text, i) where
instance From (ObsID i) i where
  from (MkObsID (x, y)) = y

-- | Smart constructor for @'ObsID'@.
makeObsID :: (From t Text) => i -> t -> ObsID i
makeObsID x y = MkObsID (into @Text y, x)

{-|
A @'Subject'@ may be mapped to zero of more observational units ('ObsUnit').
-}
data ObsUnit d i = MkObsUnit
  { obsID   :: ObsID i
  , obsData :: d
  }
  deriving (Eq, Show, Generic)

instance From (ObsID i, d) (ObsUnit d i) where
  from (x, y) = MkObsUnit x y

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
getAttritionInfo :: Cohort d i -> Maybe AttritionInfo
getAttritionInfo (MkCohort (x, _)) = x

-- | Get IDs from 'CohortData'.
getCohortDataIDs :: CohortData d i -> [ObsID i]
getCohortDataIDs (MkCohortData x) = fmap obsID x

-- | Get IDs from a cohort.
getCohortIDs :: Cohort d i -> [ObsID i]
getCohortIDs (MkCohort (_, dat)) = getCohortDataIDs dat

-- | Get data from a cohort.
getCohortDataData :: CohortData d i -> [d]
getCohortDataData (MkCohortData x) = fmap obsData x

-- | Get data from a cohort.
getCohortData :: Cohort d i -> [d]
getCohortData (MkCohort (_, dat)) = getCohortDataData dat

{-| 
A cohort specification consist of three functions: 

[@runIndices@]: 
A function which maps a 'Subject's input data,
for example a list of events,
into zero or more indices
in the form of an 'IndexSet'.

[@runCriteria@]: 
A function which maps each an index and 'Subject's input data 
into 'Criteria'.

[@runFeatures@]: 
A function which maps each an index and 'Subject's input data 
into the output data,
for example a list of 'Features.Core.Feature's
(as 'Features.Featureable's).

The evaluation of this specification is executed 
for a single 'Subject' by 'makeSubjectEvaluator' and
for a whole 'Population' by 'makeCohortEvaluator'.
The evaluation process can be modified
by certain options within a 'CohortEvalOptions',
such as 'EvaluateFeatures'. 
See 'CohortEvalOptions' for more details.
-}
data CohortSpec d1 d0 i = MkCohortSpec
  { runIndices  :: d1 -> IndexSet i
  , runCriteria :: i -> d1 -> Criteria
  , runFeatures :: i -> d1 -> d0
  }

{-|
Simply a contructor for a 'CohortSpec'.
-}
specifyCohort
  :: (d1 -> IndexSet i)
  -> (i -> d1 -> Criteria)
  -> (i -> d1 -> d0)
  -> CohortSpec d1 d0 i
specifyCohort = MkCohortSpec

{-|
Defines whether features will be evaluated,
and if so on which subjects they will be evaluated.

See 'defaultCohortEvalOptions' for default settings.
-}
data EvaluateFeatures =
  -- | This option skips the evaluation of a 'CohortSpec' 'runFeatures'.
  --   Thus the user can just run criteria
  --   in order to get a cohort's 'AttritionInfo' without computing features.
    SkipFeatures
  -- | With 'OnAll', features are run for all observational units
  --   of subjects that have an index,
  --   regardless of the 'CohortStatus' of each unit.
  | OnAll
  -- | With 'OnlyOnIncluded', features are run for all observational units
  --   of subjects that have an index,
  --   when a unit's 'CohortStatus' is 'Included'.
  | OnlyOnIncluded

{-|
Determines which subjects will be processed.
-}
data SubjectSample =
  -- | Process all subjects in a 'Population'
    AllSubjects
  -- | Include only those 'Subject's whose identifer is in the provided list.
  | SubjectIncludeList [Text]
  -- | Exclude those 'Subject's whose identifer is in the provided list.
  | SubjectExludeList [Text]
  -- | Process the first @n@ subjects
  | FirstNSubjects Int
  -- TODO: SampleNSubjects -- take a random sample of N subjects

{- (internal)
NOTE: 
in future versions of this module,
we will want to generalize the underlying type of a Population
to something other than a list.
When that time comes, 
we'll want/need the Witherable module,
so I'm (BS) am going ahead an included the dependency,
It's not strictly needed at the moment. 
-}
filterPopulation :: SubjectSample -> Population d -> Population d
filterPopulation AllSubjects x = x
filterPopulation (SubjectIncludeList inclusions) (MkPopulation x) =
  MkPopulation $ W.filter (\x -> getSubjectID x `elem` fmap into inclusions) x
filterPopulation (SubjectExludeList exclusions) (MkPopulation x) =
  MkPopulation
    $ W.filter (\x -> getSubjectID x `notElem` fmap into exclusions) x
filterPopulation (FirstNSubjects n) (MkPopulation x) = MkPopulation $ take n x

{-|
A type containing the options passed to cohort evaluators
including 'makeSubjectEvaluator' and 'makeCohortEvaluator'.
-}
data CohortEvalOptions = MkCohortEvalOptions
  {
    -- | Determines whether and how a 'CohortSpec' 'runFeatures' is evaluated.
    --   See 'EvaluateFeatures'.
    evaluateFeatures :: EvaluateFeatures
    -- | Determines which subjects will be evaluated.
    --   See 'SubjectSample'.
  , sampleSubjects   :: SubjectSample
  }

{-|
The default 'CohortEvalOptions' are:

* evaluateFeatures = 'OnlyOnIncluded':
features are evaluated only for observational units
whose 'CohortStatus' is 'Included'.

* sampleSubjects = 'AllSubjects':
all subjects are processed.
-}
defaultCohortEvalOptions :: CohortEvalOptions
defaultCohortEvalOptions = MkCohortEvalOptions OnlyOnIncluded AllSubjects

{-
*INTERNAL*

A type used in 'makeSubjectEvaluator' 
to contain a subject's processed data
before converting the result to observational units for output.
-}
data EvaluatedSubject d i =
    SNoIndex Text CohortStatus
  -- SUnits contains a list of data for each unit of a subject
  -- where the data are 
  --  (unit id, status of the unit, (maybe) units evaluated data) 
  | SUnits [ (ObsID i, CohortStatus, Maybe d) ]

instance From (ObsID i, CohortStatus, Maybe d ) (Maybe (ObsUnit d i)) where
  -- in the case that a unit's data was not evaluated
  -- return Nothing to act as as a filter for downtream processes
  from (i, _, Just d ) = Just $ MkObsUnit i d
  from (_, _, Nothing) = Nothing

instance From (EvaluatedSubject d i) [ObsUnit d i] where
  from (SNoIndex _ _) = []
  from (SUnits x    ) = W.mapMaybe (into @(Maybe (ObsUnit d i))) x

{-
*INTERNAL*
This is where the magic of processing a single 'Subject' happens.

The function converts 'CohortEvalOptions' and a 'CohortSpec'
into a function that processed a 'Subject' in some 'Monad' @m@.
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
    Nothing -> pure
      ( measureSubjectAttrition Nothing [SubjectHasNoIndex]
      , SNoIndex (into sid) SubjectHasNoIndex
      )
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
      let attrition     = measureSubjectAttrition (headMay crits) stats

      -- Create a function which applies a function g
      -- to the statusIndices and returns the desired result type
      let doFeatures g = pure (attrition, SUnits $ fmap g statusIndices)

      -- A function which takes an index and runs the 
      -- features given the cohort spec and subject's data.
      let featureRunner i = runFeatures spec i sdt

      -- Here we need to handle user options.
      case evaluateFeatures opts of
        -- the user chooses to skip evaluating features
        -- we return Nothing in the data slot.
        SkipFeatures -> doFeatures (tackOn Nothing)

        -- the user chooses to evaluate features
        -- on all observational units that have an index
        OnAll        -> do
          doFeatures (\x -> tackOn (Just $ featureRunner (from (fst x))) x)

        -- the user chooses to evaluate features
        -- only on those units whose dispositon is Included
        OnlyOnIncluded -> do
          doFeatures
            (\x -> case snd x of
              Included -> tackOn (Just $ featureRunner (from (fst x))) x
              _        -> tackOn Nothing x
            )
  where tackOn z (x, y) = (x, y, z)

{- 
*INTERNAL*

This functions processes each 'Subject' in a 'Population'
using the function created by 'makeSubjectEvaluator'.
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
  let subjects =
        into @[Subject d1] (filterPopulation (sampleSubjects opts) pop)

  pure $ (\x -> (fmap sconcat $ NE.nonEmpty $ fst x, snd x))
    (unzip (evalSubject =<< subjects))

{-|
The function converts 'CohortEvalOptions' and a 'CohortSpec'
into a function that processes a 'Population' into a 'Cohort'
within some 'Monad' @m@.

-}
makeCohortEvaluator
  :: forall m d1 d0 i
   . Monad m
  => CohortEvalOptions
  -> CohortSpec d1 d0 i
  -> (Population d1 -> m (Cohort d0 i))
makeCohortEvaluator opts spec pop =

  let evalPopulation = makePopulationEvaluator opts spec
  in  (do
        ePop <- evalPopulation pop
        pure $ MkCohort $ second
          (\x -> MkCohortData $ (into @[ObsUnit d0 i]) =<< x)
          ePop
      )

{-| 
A container hold multiple cohorts of the same type.
The key is the name of the cohort; value is a cohort.
-}
type CohortSet d i =  Map Text (Cohort d i)

{-| 
Key/value pairs of 'CohortSpec's. 
The keys are the names of the cohorts.
-}
type CohortMapSpec d1 d0 i = Map Text (CohortSpec d1 d0 i)

{-| 
Make a set of 'CohortSpec's from list input.
-}
makeCohortSpecs
  :: [(Text, d1 -> IndexSet i, i -> d1 -> Criteria, i -> d1 -> d0)]
  -> CohortMapSpec d1 d0 i
makeCohortSpecs l = fromList (fmap (\(n, i, c, f) -> (n, specifyCohort i c f)) l)

{-|
Evaluates a @'CohortSetSpec'@ on a @'Population'@
by applying the result of 'makeCohortEvaluator'
to each cohort specifying in the 'CohortSetSpec'.
-}
makeCohortSetEvaluator
  :: forall m d1 d0 i
   . Monad m
  => CohortEvalOptions
  -> CohortMapSpec d1 d0 i
  -> Population d1
  -> m (CohortSet d0 i)
makeCohortSetEvaluator opts specs pop = do
  let doCohort s = makeCohortEvaluator opts s pop
  let cohorts = fmap (\(k, v) -> (k, ) =<< doCohort v) (toList specs)
  pure $ fromList cohorts
