module Hasklepias.IntervalOperationsSpec (spec) where

import IntervalAlgebra
import Hasklepias.IntervalOperations
import Test.Hspec

containmentInt = unsafeInterval (0 :: Int) (10 :: Int)
noncontainmentInt = unsafeInterval (4 :: Int) (10 :: Int)
anotherInt = unsafeInterval (15 :: Int) (20 :: Int)

spec :: Spec 
spec = do 
    it "combineIntervals collapses overlapping intervals" $
       (combineIntervals [containmentInt, noncontainmentInt]) `shouldBe` [containmentInt]
    it "combineIntervals collapses overlapping intervals" $
       (combineIntervals [containmentInt, noncontainmentInt, anotherInt]) `shouldBe` [containmentInt, anotherInt]
    it "combineIntervals collapses overlapping intervals" $
       (combineIntervals [containmentInt]) `shouldBe` [containmentInt]
    it "combineIntervals collapses overlapping intervals" $
       (combineIntervals [noncontainmentInt]) `shouldBe` [noncontainmentInt]


    it "gaps returns gaps" $
       (gaps [containmentInt, noncontainmentInt]) `shouldBe` []
    it "gaps returns gaps" $
       (gaps [containmentInt]) `shouldBe` []
    it "gaps returns gaps" $
       (gaps [containmentInt, anotherInt]) `shouldBe` [unsafeInterval (10 :: Int) (15 :: Int)]