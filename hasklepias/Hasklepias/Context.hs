module Hasklepias.Context(
  Context
) where

import Prelude hiding (lookup)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

type ContextOf = Map

