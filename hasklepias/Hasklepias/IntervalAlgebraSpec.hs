module IntervalAlgebraSpec where

import Test.Hspec
import Test.Hspec.Core.QuickCheck
import Test.QuickCheck
import Hasklepias.IntervalAlgebra as IA
import Control.Monad

instance Arbitrary Period where
  arbitrary = liftM2 safePeriod arbitrary arbitrary

-- | Safely create a period from two Ints  
safePeriod :: Int -> Int -> Period
safePeriod x y 
  | x < y     = period x y
  | otherwise = period y x

-- | A set used for testing M1 defined so that the M1 condition is true.
data M1set = M1set { 
     i :: Period
   , j :: Period
   , k :: Period
   , l :: Period }
   deriving (Show)

instance Arbitrary M1set where
  arbitrary = do
    x <- arbitrary
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ m1set x a b c

m1set :: Period -> Int -> Int -> Int -> M1set
m1set x a b c = M1set i j k l
  where i = x
        j = expandr (abs a) $ point $ end x 
        k = expandr (abs b) $ point $ end x 
        l = expandl (abs c) $ point $ start j 

prop_IAaxiomM1 :: M1set -> Property
prop_IAaxiomM1 x = 
  (i x `meets` j x && i x `meets` k x && l x `meets` j x) ==> l x `meets` k x

-- | 
{- data M2set = M2set {
    i :: Period
  , j :: Period
  , k :: Period }
  deriving (Show)
-}


main :: IO ()
main = hspec $ do
  describe "Interval Algebra Axioms" $ modifyMaxDiscardRatio (* 10) $
    do 
      it "M1" $ property prop_IAaxiomM1
      --it "M1" $ pending
      {- 
      if two periods both meet a third, 
      then any period met by one must also be met by the other.
      -}
      it "M2" $ pending
      {- 
        if period i meets period j and period k meets l, 
        then exactly one of the following holds:
          1) i meets l; 
          2) there is an m such that i meets m and m meets l; 
          3) there is an n such that k meets nand n meets j.
      -} 

  describe "before" $ do
    it "return True for a period before another" $
      IA.before (period 0 1) (period 2 3) `shouldBe` True
    it "return False for a period after another" $
      IA.before (period 2 3) (period 0 1) `shouldBe` False
    it "return False for a period another" $
      IA.before (period 0 1) (period 1 2) `shouldBe` False

  describe "after" $ do
    it "return False for a period before another" $
      IA.before (period 0 1) (period 2 3) `shouldBe` True
    it "return True for a period after another" $
      IA.before (period 2 3) (period 0 1) `shouldBe` False
    it "return False for a period meeting another" $
      IA.before (period 0 1) (period 1 2) `shouldBe` False