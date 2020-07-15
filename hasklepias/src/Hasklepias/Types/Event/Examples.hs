{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Example Hasklepias Event Type
Description : TODO
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

module Hasklepias.Types.Event.Examples (
      exampleEvents1
    , exampleEvents2
) where
import IntervalAlgebra
import Hasklepias.Types.Event
import Hasklepias.Types.Context
import Data.List


exampleEvents1 :: Events Int
exampleEvents1 = toEvents exampleEvents1Data

exampleEvents2 :: Events Int
exampleEvents2 = toEvents exampleEvents2Data

type EventData a = (a, a, Concept)

toEvent :: (IntervalAlgebraic a) => EventData a -> Event a
toEvent x = event (unsafeInterval (t1 x) (t2 x)) (context $ [t3 x])

toEvents :: (IntervalAlgebraic a) => [EventData a] -> Events a
toEvents = sort.(map toEvent)

t1 (x , _ , _) = x
t2 (_ , x , _) = x
t3 (_ , _ , x) = x

exampleEvents1Data :: [EventData Int]
exampleEvents1Data = [
    (1,  10, "enrollment")
  , (11, 20, "enrollment")
  , (21, 30, "enrollment")
  , (31, 40, "enrollment")
  , (45, 50, "enrollment")
  , (51, 60, "enrollment")
  , (61, 63, "enrollment")
  , (71, 80, "enrollment")
  , (81, 100, "enrollment")
  , (2,  3,  "wasScratchedByCat")
  , (45, 46, "wasStruckByDuck")
  , (46, 47, "wasBitByDuck")
  , (49, 50, "wasBitByDuck")
  , (51, 52, "wasBitByDuck")
  , (89, 90, "wasBitByOrca")
  , (91, 92, "wasStuckByCow")
  , (5,  6,  "hadMinorSurgery")
  , (52, 53, "hadMajorSurgery")
  , (5,  10, "tookAntibiotics")
  , (52, 60, "wasHospitalized")
  , (52, 60, "tookAntibiotics")
  , (63, 73, "tookAntibiotics")
  , (95, 96, "died")
 ]

exampleEvents2Data :: [EventData Int]
exampleEvents2Data = [
    (1,  10, "enrollment")
  , (21, 35, "enrollment")
  , (31, 40, "enrollment")
  , (45, 59, "enrollment")
  , (51, 60, "enrollment")
  , (61, 63, "enrollment")
  , (71, 80, "enrollment")
  , (2,  3,  "wasPeckedByChicken")
  , (3,  4,  "wasPeckedByChicken")
  , (4,  5,  "wasPeckedByChicken")
  , (5,  6,  "wasPeckedByChicken")
  , (10, 11, "wasInjuredBySquirrel")
  , (15, 16, "wasDiagnosedWithSciurophobia")
  , (20, 21, "hadSquirrelContact")
  , (20, 21, "hadAnxietyAttack")
 ]
