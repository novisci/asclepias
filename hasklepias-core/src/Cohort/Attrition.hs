{-|
Module      : Hasklepias Attrition Information 
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Cohort.Attrition
  ( AttritionInfo(..)
  , AttritionLevel(..)
  , measureAttrition
  ) where

import           Cohort.Criteria
import           Control.Applicative           ( (<$>) )
import           Data.Eq                        ( Eq )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( fmap )
import           Data.List                      ( length
                                                , replicate
                                                , zip
                                                )
import           Data.Map.Strict               as Map
                                                ( Map
                                                , fromListWith
                                                , unionsWith
                                                )
import           Data.Maybe                     ( Maybe, maybe )
import           Data.Monoid                    ( mempty )
import           Data.Ord                       ( Ord(compare) )
import           Data.Semigroup                 ( Semigroup((<>)) )
import qualified Data.Set                      as Set
                                                ( Set )
import           Data.Tuple                     ( uncurry )
import           GHC.Exts                       ( IsList(..) )
import           GHC.Generics                   ( Generic )
import           GHC.Int                        ( Int )
import           GHC.Num                        ( Natural
                                                , Num((+))
                                                )
import           GHC.Show                       ( Show )

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
--   so be sure that you're combining two of the same level. 
instance Semigroup AttritionLevel where
  (<>) (MkAttritionLevel l1 c1) (MkAttritionLevel _ c2) =
    MkAttritionLevel l1 (c1 + c2)

-- | A type which collects the counts of subjects included or excluded.
data AttritionInfo = MkAttritionInfo
  { totalProcessed :: Int
  , attritionInfo  :: Set.Set AttritionLevel
  }
  deriving (Eq, Show, Generic)

-- | Takes a set of @'AttritionLevel'@ into a Map with keys of @'CohortStatus'@ and 
--   @Natural@ values.
setAttrLevlToMap :: Set.Set AttritionLevel -> Map.Map CohortStatus Natural
setAttrLevlToMap x =
  fromList $ (\(MkAttritionLevel l c) -> (l, c)) <$> toList x

-- | Takes a Map with keys of @'CohortStatus'@ and @Natural@ values (counts of each
--   status) into a set of @'AtttritionLevel'@.
mapToSetAttrLevel :: Map.Map CohortStatus Natural -> Set.Set AttritionLevel
mapToSetAttrLevel x = fromList $ uncurry MkAttritionLevel <$> toList x

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
initAttritionInfo x = fromList
  $ zip (toList (initStatusInfo x)) (replicate (length (getCriteria x)) 0)

-- | Measures @'AttritionInfo'@ from a @'Criteria'@ and a list of @'CohortStatus'@.
measureAttrition :: Maybe Criteria -> [CohortStatus] -> AttritionInfo
measureAttrition c l =
   MkAttritionInfo (length l) $ mapToSetAttrLevel $ unionsWith
    (+)
    [ maybe mempty initAttritionInfo c
    , Map.fromListWith (+) $ fmap (, 1) l
    , fromList [(SubjectHasNoIndex, 0)]
    , fromList [(Included, 0)]
      -- including SubjectHasNoIndex and Included in the case that none of the
      -- evaluated criteria have either of those statuses
    ]
