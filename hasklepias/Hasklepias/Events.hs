module Hasklepias.Events(
 Event,
 event
) where


import Hasklepias.Context
import Hasklepias.IntervalAlgebra
import Data.Sequence (Seq, (<|), (|>), (><))
import qualified Data.Sequence as Seq

newtype Event a = Event (Period, Context a)
  deriving (Show)

event :: Period -> Context a -> Event a
event i c = Event (i, c)

newtype Events a = Events (Seq a)
  deriving (Show)

events :: [Event a] -> Events a
events l = Events $ Seq.fromList l