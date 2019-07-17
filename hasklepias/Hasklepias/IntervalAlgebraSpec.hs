module IntervalAlgebraSpec where

import Test.Hspec
import Test.Hspec.Core.QuickCheck
import Test.QuickCheck
import Hasklepias.IntervalAlgebra as IA
import Control.Monad

xor a b = a /= b

instance Arbitrary Period where
  arbitrary = liftM2 safePeriod arbitrary arbitrary

-- | Safely create a period from two Ints  
safePeriod :: Int -> Int -> Period
safePeriod x y 
  | x < y     = period x y
  | otherwise = period y x

-- | Safely create a singleton Period or empty list from two Ints 
safePeriod' :: Int -> Int -> [Period]
safePeriod' x y 
  | y < x    = []
  | otherwise = [period x y]

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
        l = expandl (abs c) $ point $ begin j 

prop_IAaxiomM1 :: M1set -> Property
prop_IAaxiomM1 x = 
  (i x `meets` j x && i x `meets` k x && l x `meets` j x) ==> l x `meets` k x

-- | 
data M2set = M2set {
    ii :: Period
  , jj :: Period
  , kk :: Period
  , ll :: Period }
  deriving (Show)

instance Arbitrary M2set where
  arbitrary = do
    x <- arbitrary
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ m2set x a b c

m2set :: Period -> Period -> Int -> Int -> M2set
m2set x y a b = M2set i j k l
  where i = x
        j = expandr (abs a) $ point $ end x 
        k = y
        l = expandr (abs b) $ point $ end y

prop_IAaxiomM2 :: M2set -> Property
prop_IAaxiomM2 x =
  (ii x `meets` jj x && kk x `meets` ll x) ==> 
    (ii x `meets` ll x) `xor`  
    (not $ null m)      `xor`
    (not $ null n)
    where m = safePeriod' (end $ ii x) (begin $ ll x)
          n = safePeriod' (end $ kk x) (begin $ jj x)

prop_IAaxiomML1 :: Period -> Property
prop_IAaxiomML1 x = not (x `meets` x) === True

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
      it "M2" $ property prop_IAaxiomM2
      {- 
        if period i meets period j and period k meets l, 
        then exactly one of the following holds:
          1) i meets l; 
          2) there is an m such that i meets m and m meets l; 
          3) there is an n such that k meets n and n meets j.
      -} 
      it "ML1" $ property prop_IAaxiomML1
      {-
        for all periods cannot meet themselves 
        NOTE: Points of Period Int types violate this!
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