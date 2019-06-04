module Hasklepias.Context(
  Context,
  -- CardSuitContext,
  cardSuit
) where

import Prelude hiding (lookup)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Context

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as ContextMap

type Context a b = Context.Map String( Context.Map a b )

type CardSuit = Context.Map String String

cardSuit :: String -> Maybe CardSuit
cardSuit s 
   | s `elem` ["spades", "diamonds", "hearts", "club"] = Just (Context.fromList [("suit", s)])
   | otherwise = Nothing


