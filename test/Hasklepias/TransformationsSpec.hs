{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleContexts #-}
module Hasklepias.TransformationsSpec (spec) where

import IntervalAlgebra
    ( unsafeInterval, IntervalAlgebraic, IntervalRelation(Meets) )
import IntervalAlgebra.PairedInterval
    ( intervals, mkPairedInterval )
import Hasklepias.Transformations
import Hasklepias ( Concepts, Concept, ConceptEvent, relations, packConcept )
import Hasklepias.Types.Event.Arbitrary
import Test.Hspec ( it, shouldBe, Spec, describe )
import Test.Hspec.QuickCheck ( modifyMaxSuccess )
import Test.QuickCheck ( Testable(property), Property, (===) )
import Data.Set ( fromList )
import Data.List ( foldl, foldl', map, sort )
import Data.Text(Text)

makeState :: Text -> State
makeState c = stateConfigurator (map packConcept ["A", "B", "C"]) (fromList [packConcept c])

evpi :: Int -> Int -> State -> StateEvent Int
evpi i j s = mkPairedInterval s (unsafeInterval i j)

cevpi :: Int -> Int -> Concepts -> ConceptEvent Int
cevpi i j cs = mkPairedInterval cs (unsafeInterval i j)

checkSeqStates :: (IntervalAlgebraic i Int)=> [i Int] -> Bool
checkSeqStates x = (length x > 1) || all (== Meets) (relations x)

s0 :: [StateEvent Int]
s0 =
  [ evpi 1 10 $ makeState "A"
  , evpi 2 10 $ makeState "A"
  , evpi 5 8  $ makeState "B"
  , evpi 6 9  $ makeState "B"]

s1 :: [StateEvent Int]
s1 =
  [ evpi 1 3  $ makeState "A"
  , evpi 5 8  $ makeState "A"
  , evpi 5 10 $ makeState "A"
  , evpi 8 12 $ makeState "A"]

s2 :: [StateEvent Int]
s2 =
  [ evpi 1 3  $ makeState "A"
  , evpi 5 8  $ makeState "A"
  , evpi 5 10 $ makeState "A"
  , evpi 8 12 $ makeState "A"]

s3 :: [StateEvent Int]
s3 =
  [ evpi 1 2 $ makeState "A"
  , evpi 1 3 $ makeState "A"
  , evpi 1 4 $ makeState "A"
  , evpi 1 5 $ makeState "A"]

s4 :: [StateEvent Int]
s4 =
  [ evpi 1 6 $ makeState "A"
  , evpi 2 6 $ makeState "A"
  , evpi 3 6 $ makeState "A"
  , evpi 4 6 $ makeState "A"]

s5 :: [StateEvent Int]
s5 =
  [ evpi 1 6 $ makeState "A"
  , evpi 2 6 $ makeState "A"
  , evpi 3 4 $ makeState "B"
  , evpi 4 6 $ makeState "A"]

c0in :: [StateEvent Int]
c0in =
  [ evpi 1 10  [True, False, False]
  , evpi 2 10  [True, False, False]
  , evpi 5 8   [False, True, False]
  , evpi 6 9   [False, True, False]]
c0out :: [StateEvent Int]
c0out =
  [ evpi 1 5  [True, False, False]
  , evpi 5 9  [True, True, False]
  , evpi 9 10 [True, False, False]]

c1in :: [StateEvent Int]
c1in =
  [ evpi 1 5  [True, False, False ]
  , evpi 6 10 [False, True, False ]]
c1out :: [StateEvent Int]
c1out =
  [ evpi 1 5  [True, False, False]
  , evpi 5 6  [False, False, False]
  , evpi 6 10 [False, True, False]]

c2in :: [StateEvent Int]
c2in =
  [ evpi 1 5  [True, False, False ]
  , evpi 5 10 [False, True, False ]]
c2out :: [StateEvent Int]
c2out =
  [ evpi 1 5  [True, False, False]
  , evpi 5 10 [False, True, False]]

c3in :: [StateEvent Int]
c3in =
  [ evpi 1 5  [True, False, False ]
  , evpi 4 10 [False, True, False ]]
c3out :: [StateEvent Int]
c3out =
  [ evpi 1 4  [True, False, False]
  , evpi 4 5  [True, True, False]
  , evpi 5 10 [False, True, False]]

-- 
prop_all_meets :: [ConceptEvent Int] -> Property
prop_all_meets x = checkSeqStates (transformToMeetingSequence (map packConcept ["B" , "C"]) (sort x)) === True

spec :: Spec
spec = do
    describe "Property tests of transformToMeetingSequence" $
      modifyMaxSuccess (*10) $
      it "prop_all_meets holds" $ property prop_all_meets
    
    describe "a few unit tests that meetEvents returns meeting events" $
      do
        it "s0" $ checkSeqStates (intervals $ meetEvents s0) `shouldBe` True
        it "s1" $ checkSeqStates (intervals $ meetEvents s1) `shouldBe` True
        it "s2" $ checkSeqStates (intervals $ meetEvents s2) `shouldBe` True
        it "s3" $ checkSeqStates (intervals $ meetEvents s3) `shouldBe` True
        it "s4" $ checkSeqStates (intervals $ meetEvents s4) `shouldBe` True
        it "s5" $ checkSeqStates (intervals $ meetEvents s5) `shouldBe` True

    describe "" $
        do
            it "c0" $ stateConfigurator (map packConcept ["A", "B", "C"]) (fromList [packConcept "C"])
                `shouldBe` [False, False, True]
            it "c0" $ stateConfigurator (map packConcept ["A", "B", "C"]) (fromList [packConcept "D"])
                `shouldBe` [False, False, False]
            it "c0" $ stateConfigurator (map packConcept ["A", "B", "C"]) (fromList (map packConcept ["A", "B"]))
                `shouldBe` [True, True, False]

    describe "" $
        do it "meetEvents 0" $
             meetEvents c0in `shouldBe` c0out
           it "meetEvents 1"$
             meetEvents c1in `shouldBe` c1out
           it "meetEvents 2"$
             meetEvents c2in `shouldBe` c2out
           it "meetEvents 3"$
             meetEvents c3in `shouldBe` c3out

    describe "" $
        do
            it "combine equal" $ mergeStateEvents (evpi 0 10 [True, False]) (evpi 0 10 [False, True])
                `shouldBe` [evpi 0 10 [True, True]]

            it "combine meets 1" $ mergeStateEvents (evpi 0 5 [True, False]) (evpi 5 10 [True, True])
                `shouldBe` [evpi 0 5 [True, False], evpi 5 10 [True, True]]

            it "combine overlaps 1" $ mergeStateEvents (evpi 2 10 [True, False]) (evpi 5 12 [False, True])
                `shouldBe` [evpi 2 5 [True, False], evpi 5 10 [True, True], evpi 10 12 [False, True]]
            it "combine overlaps 2" $ mergeStateEvents (evpi 2 10 [True, True]) (evpi 5 12 [False, True])
                `shouldBe` [evpi 2 10 [True, True], evpi 10 12 [False, True]]
            it "combine overlaps 3" $ mergeStateEvents (evpi 2 10 [True, False]) (evpi 5 12 [True, True])
                `shouldBe` [evpi 2 5 [True, False], evpi 5 12 [True, True]]

            it "combine finishedBy 1" $ mergeStateEvents (evpi 0 10 [True, False]) (evpi 2 10 [False, True])
                `shouldBe` [evpi 0 2 [True, False], evpi 2 10 [True, True]]
            it "combine finishedBy 2" $ mergeStateEvents (evpi 0 10 [True, True]) (evpi 2 10 [False, True])
                `shouldBe` [evpi 0 10 [True, True]]
            it "combine finishedBy 3" $ mergeStateEvents (evpi 0 10 [False, True]) (evpi 2 10 [True, True])
                `shouldBe` [evpi 0 2 [False, True], evpi 2 10 [True, True]]

            it "combine contains 1" $ mergeStateEvents (evpi 2 10 [True, False]) (evpi 5 8 [False, True])
                `shouldBe` [evpi 2 5 [True, False], evpi 5 8  [True, True], evpi 8 10 [True, False]]
            it "combine contains 2" $ mergeStateEvents (evpi 2 10 [True, True]) (evpi 5 8 [False, True])
                `shouldBe` [evpi 2 10 [True, True]]

            it "combine starts 1" $ mergeStateEvents (evpi 0 5 [True, False]) (evpi 0 10 [False, True])
                `shouldBe` [evpi 0 5 [True, True], evpi 5 10 [False, True]]
            it "combine starts 2" $ mergeStateEvents (evpi 0 5 [True, True]) (evpi 0 10 [False, True])
                `shouldBe` [evpi 0 5 [True, True], evpi 5 10 [False, True]]
            it "combine starts 3" $ mergeStateEvents (evpi 0 5 [True, False]) (evpi 0 10 [True, True])
                `shouldBe` [evpi 0 10 [True, True]]
            it "combine starts 4" $ mergeStateEvents (evpi 1 10 [True, False]) (evpi 2 10 [True, True])
                `shouldBe` [evpi 1 2 [True, False], evpi 2 10 [True, True]]



