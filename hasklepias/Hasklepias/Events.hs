module Hasklepias.Events(
 Event,
 event
) where


import Prelude hiding (lookup)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Context

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntContext
--import Hasklepias.Context
import Hasklepias.IntervalAlgebra

newtype Pair a b = Pair { getPair :: (a, b) } deriving (Show)

type MapSS = Map String String

newtype Event = Event ( Pair Period MapSS )
  deriving (Show)

event :: Period -> MapSS -> Event
event i c = Event (Pair (i, c))