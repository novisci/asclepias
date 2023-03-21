{-|
Module      : Cohort output
Description : Methods for outputting a cohort
Copyright   : (c) Target RWE 2023
License     : BSD3
Maintainer  : bbrown@targetrwe.com
              ljackman@targetrwe.com
              dpritchard@targetrwe.com
-}

{-# LANGUAGE DeriveGeneric #-}

module Cohort.Output where

import           Cohort.Cohort
import           Data.Aeson        (ToJSON (..), Value)
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as M
import           Data.Text         (Text)
import           GHC.Generics      (Generic)
import           Variable.Variable

-- | Internal. Type controling the output shape of a 'Cohort'.
data CohortJSON
  = MkCohortJSON
      { attritionJSON :: AttritionInfo
      , cohortJSON    :: [ObsUnitJSON]
      }
  deriving (Eq, Generic, Show)

instance ToJSON CohortJSON

-- | Internal. Type controling the output shape of each 'ObsUnit'.
data ObsUnitJSON
  = MkObsUnitJSON
      { obsIdJSON       :: Value
      , variableRowJSON :: [VariableWrapped]
      }
  deriving (Eq, Generic, Show)

instance ToJSON ObsUnitJSON

-- | Internal. Convert 'ObsUnit' to 'ObsUnitJSON' using the 'ToJSON' instances
-- for 'ObsId' and 'VariableWrapped', to which the 'obsData' are first
-- converted.
toObsUnitJSON :: (ToJSON a) => ObsUnit a -> ObsUnitJSON
toObsUnitJSON ou = MkObsUnitJSON (toJSON (obsId ou)) dt
  where dt = map asVariableWrapped (obsData ou)

-- | Internal. Convert each 'Cohort' in a 'CohortMap' to 'CohortJSON'.
toCohortJSON :: (ToJSON a) => CohortMap a -> CohortMapJSON
toCohortJSON chtm = let f cht = MkCohortJSON (attritionInfo cht) (map toObsUnitJSON (cohortData cht))
                    in M.map f chtm


-- | Similar to 'CohortMap', but where the 'Cohort's have been mapped to a
-- 'CohortJSON'.
type CohortMapJSON = Map Text CohortJSON
