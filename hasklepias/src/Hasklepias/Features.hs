{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-|
Module      : Hasklepias Features
Description : TODO
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

module Hasklepias.Features(
    Feature(..),
    firstOccurrenceOfConcept
) where

import IntervalAlgebra
import Hasklepias.Types.Event
import Hasklepias.Types.Context
import Hasklepias.IntervalOperations


-- | TODO
data Feature a = 
    Deficient  {  getReason  :: String } 
  | Sufficient {  getName    :: String
                , getData    :: a }
    deriving (Show, Eq)

instance Functor Feature where
  fmap f (Deficient  r)   = Deficient r
  fmap f (Sufficient n d) = Sufficient n (f d)

-- data FeatureMaker a b c =
--       EF  (Events a  -> Feature c)
--     | EFF (Events a  -> Feature b -> Feature c)
--     | FF  (Feature b -> Feature c)

-- defineFeature :: String -> (FeatureMaker a b c)
-- defineFeature name = 



-- defineFeature1 :: String -> (Events -> Maybe a) -> (Events -> Feature a)
-- defineFeature1 label derivation = 
--   \e -> 
--   case derivation e of 
--     Just x  -> (Right' x label)
--     Nothing -> (Left' "TODO: add some Some reason")

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

firstOccurrenceOfConcept :: (IntervalAlgebraic a) =>
                Concept -> Events a -> Maybe (Event a)
firstOccurrenceOfConcept c es = 
    safeHead.(filterEvents (\x -> x `hasConcept` c)) $ es



