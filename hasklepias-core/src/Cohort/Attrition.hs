{-|
Module      : Hasklepias Attrition Information 
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Cohort.Attrition
  ( AttritionInfo(..)
  , AttritionLevel(..)
  , measureSubjectAttrition
  ) where

import           Cohort.Criteria
import           Data.Map.Strict               as Map
                                                ( Map
                                                , fromListWith
                                                , unionsWith
                                                )
import qualified Data.Set                      as Set
                                                ( Set )
import           GHC.Exts                       ( IsList(..) )
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( Natural )

-- | A type which collects counts of a 'CohortStatus'
data AttritionLevel = MkAttritionLevel
  { attritionLevel :: CohortStatus
  , attritionCount :: Natural
  }
  deriving (Eq, Show, Generic)

-- | Ordering of @AttritionLevel@ is based on the value of its 'attritionLevel'. 
instance Ord AttritionLevel where
  compare (MkAttritionLevel l1 _) (MkAttritionLevel l2 _) = compare l1 l2

-- | NOTE: the @Semigroup@ instance prefers the 'AttritionLevel' from the left,
--   so be sure that you're combining two of the same level. 
instance Semigroup AttritionLevel where
  (<>) (MkAttritionLevel l1 c1) (MkAttritionLevel _ c2) =
    MkAttritionLevel l1 (c1 + c2)

-- | A type which collects the counts of subjects included or excluded.
data AttritionInfo = MkAttritionInfo
  { totalSubjectsProcessed :: Int
  , totalUnitsProcessed    :: Int
  , attritionInfo          :: Set.Set AttritionLevel
  }
  deriving (Eq, Show, Generic)

-- | Takes a set of @'AttritionLevel'@ into a Map with keys of @'CohortStatus'@ and 
--   @Natural@ values.
setAttrLevlToMap :: Set.Set AttritionLevel -> Map.Map CohortStatus Natural
setAttrLevlToMap x =
  fromList $ (\(MkAttritionLevel l c) -> (l, c)) <$> toList x

-- | Takes a Map with keys of @'CohortStatus'@ and @Natural@ values (counts of each
--   status) into a set of @'AttritionLevel'@.
mapToSetAttrLevel :: Map.Map CohortStatus Natural -> Set.Set AttritionLevel
mapToSetAttrLevel x = fromList $ uncurry MkAttritionLevel <$> toList x

-- | Two @AttritionInfo@ values can be combined, but this meant for combining
--   attrition info from the same set of @Criteria@.
instance Semigroup AttritionInfo where
  (<>) (MkAttritionInfo s1 u1 i1) (MkAttritionInfo s2 u2 i2) = MkAttritionInfo
    (s1 + s2)
    (u1 + u2)
    ( mapToSetAttrLevel
    $ unionsWith (+) [setAttrLevlToMap i1, setAttrLevlToMap i2]
    )

-- Initializes @AttritionInfo@ from a @'Criteria'@.
initAttritionInfo :: Criteria -> Map.Map CohortStatus Natural
initAttritionInfo x = fromList
  $ zip (toList (initStatusInfo x)) (replicate (length (getCriteria x)) 0)

{- |
Measures @'AttritionInfo'@ from a @'Criteria'@ and a list of @'CohortStatus'@
**for a single subject**.
The 'AttritionInfo' across subjects can obtains by summing
a list of 'AttritionInfo'.

A note on why this function takes 'Maybe Criteria' as input:
A subject may not have an 'Criteria' 
if their only 'CohortStatus'is 'SubjectHasNoIndex. 
However, a 'Criteria' is needed to initialize a 'Map.Map CohortStatus Natural'
with 'initAttritionInfo'.
-}
measureSubjectAttrition :: Maybe Criteria -> [CohortStatus] -> AttritionInfo
measureSubjectAttrition mcriteria statuses =
  MkAttritionInfo
    -- again, function is meant to be used on a single subject, so one
                  1
    -- number of units is number of statuses not equal to SubjectHasNoIndex 
                  (length $ filter (/= SubjectHasNoIndex) statuses)
    -- attritionInfo is formed by unioning via a Map in order to 
    -- sum within each CohortStatus
    $ mapToSetAttrLevel
    $ unionsWith
        (+)
        [ maybe mempty initAttritionInfo mcriteria
        , Map.fromListWith (+) $ fmap (, 1) statuses
        , fromList [(SubjectHasNoIndex, 0)]
        , fromList [(Included, 0)]
      -- including SubjectHasNoIndex and Included for the cases that none of the
      -- evaluated criteria have either of those statuses
        ]
