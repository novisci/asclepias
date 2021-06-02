{-|
Module      : Example Hasklepias Events
Description : TODO
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
-}
{-# LANGUAGE OverloadedStrings #-}

module ExampleEvents (
      exampleEvents1
    , exampleEvents2
    , exampleEvents3
    , exampleEvents4
    , exampleSubject1
    , exampleSubject2
) where

import Hasklepias

exampleEvents1 :: Events Int
exampleEvents1 = toEvents exampleEvents1Data

exampleEvents2 :: Events Int
exampleEvents2 = toEvents exampleEvents2Data

exampleEvents3 :: Events Int 
exampleEvents3 = toEvents exampleEvents3Data

exampleEvents4 :: Events Int 
exampleEvents4 = toEvents exampleEvents4Data

exampleSubject1 :: Subject (Events Int)
exampleSubject1 = MkSubject ("a", exampleEvents1)

exampleSubject2 :: Subject (Events Int)
exampleSubject2 = MkSubject ("b", exampleEvents2)

type EventData a = (a, a, Text)

toEvent :: (IntervalSizeable a a, Show a) => EventData a -> Event a
toEvent x = event (beginerval (t1 x) (t2 x)) (context Nothing (packConcepts [t3 x]))

toEvents :: (Ord a, Show a, IntervalSizeable a a) => [EventData a] -> Events a
toEvents = sort.map toEvent

t1 :: (a, b, c) -> a
t1 (x , _ , _) = x
t2 :: (a, b, c) -> b
t2 (_ , x , _) = x
t3 :: (a, b, c) -> c
t3 (_ , _ , x) = x

exampleEvents1Data :: [EventData Int]
exampleEvents1Data = [
    (9, 1,   "enrollment")
  , (9, 11,  "enrollment")
  , (9, 21,  "enrollment")
  , (9, 31,  "enrollment")
  , (5, 45,  "enrollment")
  , (9, 51,  "enrollment")
  , (2, 61,  "enrollment")
  , (9, 71,  "enrollment")
  , (19, 81, "enrollment")
  , (1, 2,   "wasScratchedByCat")
  , (1, 45,  "wasStruckByDuck")
  , (1, 46,  "wasBitByDuck")
  , (1, 49,  "wasBitByDuck")
  , (1,  51, "wasBitByDuck")
  , (1, 60,  "wasBitByOrca")
  , (1, 91,  "wasStuckByCow")
  , (1, 5,   "hadMinorSurgery")
  , (1, 52,  "hadMajorSurgery")
  , (5, 5,   "tookAntibiotics")
  , (8, 52,  "wasHospitalized")
  , (6, 45,  "tookAntibiotics")
  , (13, 60, "tookAntibiotics")
  , (3, 80,  "tookAntibiotics")
  , (1, 95,  "died")
 ]

exampleEvents2Data :: [EventData Int]
exampleEvents2Data = [
    (9, 1,   "enrollment")
  , (14, 21, "enrollment")
  , (9, 31,  "enrollment")
  , (14, 45, "enrollment")
  , (9, 60,  "enrollment")
  , (2, 61,  "enrollment")
  , (9, 80,  "enrollment")
  , (1, 2,   "wasPeckedByChicken")
  , (1, 3,   "wasPeckedByChicken")
  , (1, 4,   "wasPeckedByChicken")
  , (1, 5,   "wasPeckedByChicken")
  , (1, 10,  "wasInjuredBySquirrel")
  , (1, 15,  "wasDiagnosedWithSciurophobia")
  , (1, 20,  "hadSquirrelContact")
  , (1, 20,  "hadAnxietyAttack")
 ]

exampleEvents3Data :: [EventData Int]
exampleEvents3Data = [
    (9, 1,   "enrollment")
  , (9, 11,  "enrollment")
  , (9, 21,  "enrollment")
  , (9, 31,  "enrollment")
  , (5, 45,  "enrollment")
  , (9, 51,  "enrollment")
  , (2, 61,  "enrollment")
  , (9, 71,  "enrollment")
  , (19, 81, "enrollment")
  , (1, 2,   "wasScratchedByCat")
  , (1, 45,  "wasStruckByDuck")
  , (1, 46,  "wasBitByDuck")
  , (1, 49,  "wasBitByDuck")
  , (1, 51,  "wasBitByDuck")
  , (1, 60,  "wasBitByOrca")
  , (1, 91,  "wasStuckByCow")
  , (1, 5,   "hadMinorSurgery")
  , (1, 52,  "hadMajorSurgery")
  , (5, 5,   "tookAntibiotics")
  , (8, 52,  "wasHospitalized")
  , (10, 45, "tookAntibiotics")
  , (15, 58, "tookAntibiotics")
  , (3, 80,  "tookAntibiotics")
  , (1, 95,  "died") 
 ]

exampleEvents4Data :: [EventData Int]
exampleEvents4Data = [
    (1, 1,   "c1")
  , (3, 11,  "c1")
  , (9, 16,  "c1")
  , (9, 31,  "c1")
  , (5, 45,  "c1")
  , (1, 10,  "c2")
  , (1, 13,  "c2")
 ]