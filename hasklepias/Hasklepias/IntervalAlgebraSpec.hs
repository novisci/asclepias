module IntervalAlgebraSpec where

import Test.Hspec
import Test.Hspec.Core.QuickCheck
import Test.QuickCheck
import Hasklepias.IntervalAlgebra as IA
import Control.Monad

xor :: Bool -> Bool -> Bool
xor a b = a /= b

instance Arbitrary Period where
  arbitrary = liftM2 safePeriod arbitrary arbitrary

-- | Safely create a period from two Ints  
safePeriod :: Int -> Int -> Period
safePeriod x y 
  | x < y     = period x (y + 1)
  | otherwise = period y (x + 1)

-- | Safely create a singleton Period or empty list from two Ints 
safePeriod' :: Int -> Int -> [Period]
safePeriod' x y 
  | y < x    = []
  | otherwise = [period x (y + 1)]

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
m1set x a b c = M1set p1 p2 p3 p4
  where p1 = x
        p2 = expandr ((abs a) + 1) $ point $ end x 
        p3 = expandr ((abs b) + 1) $ point $ end x 
        p4 = expandl ((abs c) + 1) $ point $ begin p2 
  -- NOTE: adding 1 to the expansion is to ensure that the intervals have
  -- a nonzero duration (i.e. that the periods are not points).
  -- Consider the following falsification:
  --    Falsified (after 1 test):
  --       M1set {i = (-1, 0), j = (0, 1), k = (0, 0), l = (0, 0)}
  -- Here k == l and both have duration 0, but this violates ML1. 
  -- My (B. Saul) current (2019-07-17) understanding is that these axioms
  -- only apply to *interval* time and not to points; hence the falsification
  -- shown is not a valid test of the axiom since it includes points.


prop_IAaxiomM1 :: M1set -> Property
prop_IAaxiomM1 x = 
  (i x `meets` j x && i x `meets` k x && l x `meets` j x) ==> (l x `meets` k x)

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
m2set x y a b = M2set p1 p2 p3 p4
  where p1 = x
        p2 = expandr (abs a) $ point $ end x 
        p3 = y
        p4 = expandr (abs b) $ point $ end y

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

prop_IAaxiomML2 :: M2set -> Property
prop_IAaxiomML2 x =
  (ii x `meets` jj x) ==> not (jj x `meets` ii x)

prop_IAaxiomM3 :: Period -> Property
prop_IAaxiomM3 x = 
   (y `meets` x && x `meets` z) === True
   where y = expandl 1 $ point $ begin x
         z = expandr 1 $ point $ end x

prop_IAaxiomM4 :: M2set -> Property
prop_IAaxiomM4 x = 
   ((pm `meets` ii x && ii x `meets` jj x && jj x `meets` pn) &&
   (pm `meets` pk && pk `meets` pn)) === True
   where pm = expandl 1 $ point $ begin $ ii x
         pn = expandr 1 $ point $ end $ jj x
         pk = period (end pm) (begin pn)

prop_IAbefore :: Period -> Period -> Property
prop_IAbefore x y = 
  IA.before x y ==> (x `meets` z) && (z `meets` y)
    where z = period (end x) (begin y)

main :: IO ()
main = hspec $ do
  describe "Interval Algebra Axioms" $ --modifyMaxDiscardRatio (* 10) $
    do 
      it "M1" $ property prop_IAaxiomM1
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
        a period cannot meet itself
      -}
      it "ML2" $ property prop_IAaxiomML2
      {-
        if i meets j then j does not meet i
      -}
      -- it "ML3"
      {-
        For all i, there does not exist m such that i meets m and m meet i
        Not testing that this axiom holds, as I'm not sure how I would
      -}
      it "M3" $ property prop_IAaxiomM3
      {-
        for all periods i there exist periods j and k such that j:i:k
      -}
      it "M4" $ property prop_IAaxiomM4
      {-
        if i meets j then there exists k, m, n such that m:i:j:n and m:k:n
      -}
      it "M5" $ pending 
      {-
      -}

  describe "Interval Algebra operators" $
   do
      it "IAbefore" $ property prop_IAbefore

  {-
  describe "before" $ do
    it "return True for a period before another" $
      IA.before (period 0 1) (period 2 3) `shouldBe` True
    it "return False for a period after another" $
      IA.before (period 2 3) (period 0 1) `shouldBe` False
    it "return False for a period another" $
      IA.before (period 0 1) (period 1 2) `shouldBe` False
  -}

  describe "after" $ do
    it "return False for a period before another" $
      IA.before (period 0 1) (period 2 3) `shouldBe` True
    it "return True for a period after another" $
      IA.before (period 2 3) (period 0 1) `shouldBe` False
    it "return False for a period meeting another" $
      IA.before (period 0 1) (period 1 2) `shouldBe` False